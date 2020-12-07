{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Data.Functor
import Data.Maybe
import Data.List

type BagType = String 
type BagQuantity = (Int, BagType)
type Rule = (BagType, [BagQuantity])

(<:>) :: Applicative f => f a -> f [a] -> f [a]
fa <:> fas = (:) <$> fa <*> fas

(<\>) :: Parsec s u a -> Parsec s u a -> Parsec s u a 
p <\> p' = try p <|> p'

(<~>) :: Parsec s u a -> Parsec s u b -> (Parsec s u (a, b))
p <~> p' = (,) <$> p <*> p'

lookUp :: Eq a => a -> [(a, b)] -> (a, b)
lookUp a = fromJust . find ((a ==) . fst) 

word :: Parsec String u String 
word = many (noneOf " \n")

bag :: Parsec String u BagType
bag = (++) <$> (word <* oneOf " ") <*> (word)

rule :: Parsec String u Rule
rule = (,) <$> (bag <* string " bags contain ") <*> ((noBags $> []) <\> (bagQuantities)) <* char '.'
  where
    noBags = string "no other bag" <* optional (char 's')
    bagQuantities = bagQuantity `sepBy` string ", "
    quantity = read @Int <$> many1 digit
    bagQuantity = quantity <~> (char ' ' *> bag <* string " bag" <* optional (char 's'))

getRule :: String -> Rule
getRule = either (const (error ":(")) id . runParser rule () ""

containerOf :: [Rule] -> BagType -> [BagType]
containerOf rs b = map (snd) (snd $ lookUp b rs) 

directlyContainedBy :: [Rule] -> BagType -> BagType -> Bool
directlyContainedBy rs b b' = b `elem` containerOf rs b'

-- TODO: make this dynamic
transitivelyContainedBy :: [Rule] -> BagType -> BagType -> Bool 
transitivelyContainedBy rs b b' = directly || indirectly
  where 
    directly   = directlyContainedBy rs b b'
    indirectly = any (transitivelyContainedBy rs b) (containerOf rs b')

getContainers :: [Rule] -> BagType -> [BagType]
getContainers rs b = filter (transitivelyContainedBy rs b) (map fst rs)

totalNumBags :: [Rule] -> BagType -> Int
totalNumBags rs b
  | (_, []) <- lookUp b rs = 1
  | (_, qs) <- lookUp b rs = succ $ sum $ map (\(n, b') -> n * totalNumBags rs b') qs

-- TODO: this is SLOW
partOne :: [Rule] -> Int 
partOne rs = length $ getContainers rs "shinygold"

-- -1 since we dont count the topmost bag
partTwo :: [Rule] -> Int 
partTwo rs = totalNumBags rs "shinygold" - 1

main :: IO ()
main = do 
  contents <- readFile "input"
  let rules = map getRule (lines contents)
  -- print (partOne rules) 
  print (partTwo rules)