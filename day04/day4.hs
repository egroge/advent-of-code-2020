import Data.List (nub, sort, takeWhile, all)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Text.Regex.PCRE

data FieldKey = Ecl | Pid | Eyr | Hcl | Byr | Iyr | Cid | Hgt deriving (Eq, Show)

type Field = (FieldKey, String)
type Passport = [Field]

requiredFields :: [FieldKey]
requiredFields = [Ecl, Pid, Eyr, Hcl, Byr, Iyr, Hgt]

-- PRE input is valid format
field :: String ->  Field
field s = case splitOn ":" s of
            ["ecl", rem] -> (Ecl, rem) 
            ["pid", rem] -> (Pid, rem) 
            ["eyr", rem] -> (Eyr, rem) 
            ["hcl", rem] -> (Hcl, rem) 
            ["byr", rem] -> (Byr, rem) 
            ["iyr", rem] -> (Iyr, rem) 
            ["cid", rem] -> (Cid, rem) 
            ["hgt", rem] -> (Hgt, rem) 

passport :: [String] -> Passport
passport = nub . map field 

getPassports :: String -> IO [Passport]
getPassports raw = return $ map passport entries 
  where 
    entries = map words $ splitOn "\n\n" raw

passportHasRequiredFields :: Passport -> Bool 
passportHasRequiredFields p = all (`elem` ks) requiredFields
  where 
    ks = map fst p

fieldValid :: Field -> Bool 
fieldValid (k, v) = case k of 
                      Byr -> stringBetween v 1920 2002
                      Iyr -> stringBetween v 2010 2020
                      Eyr -> stringBetween v 2020 2030
                      Hgt -> validHeight
                      Hcl -> v =~ "^#[\\da-f]{6}$" :: Bool
                      Ecl -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                      Pid -> length v == 9 && isJust (asNum v)
                      Cid -> True
  where
    validHeight = case v =~ "^(\\d\\d\\d)cm$|^(\\d\\d)in$" :: [[String]] of 
                    [[_, "", n]] -> stringBetween n 59 76
                    [[_, n, ""]] -> stringBetween n 150 193
                    _ -> False 

    asNum s = readMaybe s :: Maybe Int
    stringBetween s l u = case asNum s of 
                            Just n  -> l <= n && n <= u
                            Nothing -> False 

passportValid :: Passport -> Bool 
passportValid p = all fieldValid p && passportHasRequiredFields p

partOne :: [Passport] -> Int 
partOne = length . filter passportHasRequiredFields

partTwo :: [Passport] -> Int
partTwo = length . filter passportValid

-- Answers: 228 175
main :: IO () 
main = do 
  contents  <- readFile "input.txt"
  passports <- getPassports contents
  print $ partOne passports
  print $ partTwo passports