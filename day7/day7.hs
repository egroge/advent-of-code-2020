{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Data.Functor
import Data.Maybe
import Data.List
import Data.Array

type BagType = String 
type BagQuantity = (Int, BagType)
type Rule = (BagType, [BagQuantity])

(<:>) :: Applicative f => f a -> f [a] -> f [a]
fa <:> fas = (:) <$> fa <*> fas

infixl 3 <\>
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
rule = (bag <* string " bags contain ") <~> ((noBags $> []) <\> bagQuantities) <* char '.'
  where
    noBags = string "no other bag" <* optional (char 's')
    bagQuantities = bagQuantity `sepBy` string ", "
    quantity = read @Int <$> many1 digit
    bagQuantity = quantity <~> (char ' ' *> bag <* string " bag" <* optional (char 's'))

getRule :: String -> Rule
getRule = either (const (error ":(")) id . runParser rule () ""

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a 
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

-- TODO this is now dynamic, but kinda gross
getContainers :: [Rule] -> BagType -> [BagType]
getContainers rs b = map fst $ filter (\(b', l) -> b `elem` l) allContainers
  where 
    allContainers :: [(BagType, [BagType])]
    allContainers = map (\n -> (fst (sorted !! n), arr ! n)) [0.. length sorted - 1]

    sorted = sortBy (\x y -> compare (length (snd x)) (length (snd y))) rs
    ind b' = fromJust $ findIndex (== b') (map fst sorted)
    arr = tabulate (0, length sorted) memo
    memo i 
      | qs == []  = [] 
      | otherwise = others ++ concatMap ((arr !) . ind) others
      where 
        others = map snd qs
        qs     = snd (sorted !! i)

totalNumBags :: [Rule] -> BagType -> Int
totalNumBags rs b
  | (_, []) <- lookUp b rs = 1
  | (_, qs) <- lookUp b rs = succ $ sum $ map (\(n, b') -> n * totalNumBags rs b') qs

partOne :: [Rule] -> Int 
partOne rs = length $ getContainers rs "shinygold"

-- -1 since we dont count the topmost bag
partTwo :: [Rule] -> Int 
partTwo rs = totalNumBags rs "shinygold" - 1

main :: IO ()
main = do 
  contents <- readFile "input"
  let rules = map getRule (lines contents)
  print (partOne rules) 
  print (partTwo rules)