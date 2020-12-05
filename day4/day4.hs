{-# LANGUAGE TypeApplications #-}

import Text.Parsec hiding (between)
import Data.Functor
import Control.Monad
import Control.Applicative (empty)

data Height = In Int | Cm Int deriving Show
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving Show

type HairColor = String
type Year = Int

data Field = Ecl EyeColor 
  | Pid String 
  | Eyr Year 
  | Hcl HairColor 
  | Byr Year 
  | Iyr Year 
  | Hgt Height
  | Cid 
  deriving Show

instance Eq Field where 
  Ecl _ == Ecl _ = True
  Pid _ == Pid _ = True
  Eyr _ == Eyr _ = True
  Hcl _ == Hcl _ = True
  Byr _ == Byr _ = True
  Iyr _ == Iyr _ = True
  Hgt _ == Hgt _ = True
  Cid   == Cid   = True
  _     == _     = False

type Passport = [Field]

between :: Int -> Int -> Int -> Bool 
between l u n = l <= n && n <= u

infixl 3 <\>
(<\>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p <\> p' = try p <|> p'

height :: Parsec String u Height
height = (Cm <$> cms >?> between 150 193) <\> (In <$> inches >?> between 59 76) 
  where 
    inches = (read @Int) <$> (sequence [digit, digit] <* string "in")
    cms    = (read @Int) <$> (sequence [digit, digit, digit] <* string "cm")

eyeColor :: Parsec String u EyeColor
eyeColor = string "amb" $> Amb <\>
  string "blu" $> Blu <\>
  string "brn" $> Brn <\>
  string "gry" $> Gry <\>
  string "grn" $> Grn <\>
  string "hzl" $> Hzl <\>
  string "oth" $> Oth  

(>?>) :: Parsec s u a -> (a -> Bool) -> Parsec s u a
p >?> f = p >>= (\x -> if f x then pure x else empty)

yearBetween :: Year -> Year -> Parsec String u Year
yearBetween l u = ((read @Int) <$> replicateM 4 digit) >?> between l u

ecl :: Parsec String u Field
ecl = string "ecl:" *> (Ecl <$> eyeColor)

pid :: Parsec String u Field
pid = string "pid:" *> (Pid <$> replicateM 9 digit)

eyr :: Parsec String u Field
eyr = string "eyr:" *> (Eyr <$> yearBetween 2020 2030)

byr :: Parsec String u Field
byr = string "byr:" *> (Byr <$> yearBetween 1920 2002)

iyr :: Parsec String u Field
iyr = string "iyr:" *> (Iyr <$> yearBetween 2010 2020)

hcl :: Parsec String u Field 
hcl = string "hcl:" *> (Hcl <$> hairColor)
  where 
    hairColor = char '#' *> replicateM 6 (oneOf "abcdef0123456789")

hgt :: Parsec String u Field 
hgt = string "hgt:" *> (Hgt <$> height)

cid :: Parsec String u Field
cid = (string "cid:" *> many (noneOf " ")) $> Cid

field :: Parsec String u Field
field = ecl <\> pid <\> eyr <\> byr <\> iyr <\> hcl <\> hgt <\> cid

passport :: Parsec String u Passport
passport = field `sepBy1` try (oneOf " \n" *> notFollowedBy (char '\n'))

(<:>) :: Applicative f => f a -> f[a] -> f [a]
(<:>) x xs = (:) <$> x <*> xs

passports :: Parsec String u [Passport]
-- passports = passport `sepBy` string "\n\n" 
passports = passport <:> (passports <|> (eof $> []))

getPassports :: String -> IO [Passport]
getPassports s = return res'
  where 
    res' = either (const []) id res
    res  = runParser passports () "" s

partTwo :: [Passport] -> Int 
partTwo ps = length $ filter passportHasRequiredFields ps

-- TODO this is a horrible way to do this
passportHasRequiredFields :: Passport -> Bool 
passportHasRequiredFields p = all (`elem` p) requiredFields 
  where 
    requiredFields = [Ecl Amb, Pid "", Eyr 0, Hcl "", Byr 0, Iyr 0, Hgt (In 0)]

test :: Parsec String () a -> String -> Either ParseError a
test p = runParser p () ""

-- Answers: 228 175
main :: IO () 
main = do 
  contents  <- readFile "valid.txt"
  passports <- getPassports contents
  -- print $ partOne passports
  print passports
  print $ partTwo passports