{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

import Text.Parsec hiding (getInput, between)
import Data.List (find, sortOn, isPrefixOf, findIndices, (\\), delete)
import Data.Maybe (mapMaybe, isNothing)

type Field = String
type Range = (Int, Int)
type Rule = (Field, (Range, Range))
type Ticket = [Int]
type RulePossibilities = (Rule, [Int])

(<~>) :: Parsec s u a -> Parsec s u b -> Parsec s u (a, b) 
p <~> ps = (,) <$> p <*> ps

num :: Parsec String u Int
num = (read @Int) <$> many digit

rule :: Parsec String u Rule 
rule = ((many (noneOf ":\n") <* string ": ") <~> ((range <* string " or ") <~> range)) <* char '\n'
  where 
    range = (num <* char '-') <~> num

ticket :: Parsec String u Ticket
ticket = (num `sepBy` char ',') <* char '\n'

getInput :: String -> IO ([Rule], Ticket, [Ticket])
getInput s = either (const (error "Problem parsing input")) return (runParser p () "" s)
  where 
    p = (,,) <$> (many rule <* char '\n') 
               <*> (string "your ticket:\n" *> ticket) 
               <*> (string "\nnearby tickets:\n" *> many ticket)

matchesRule :: Rule -> Int -> Bool 
matchesRule (_, ((a, b), (a', b'))) n = (a <= n && n <= b) || (a' <= n && n <= b')

findInvalidNumber :: Ticket -> [Rule] -> Maybe Int
findInvalidNumber t rs = find (not . matchAnyRule) t
  where 
    matchAnyRule n = any (flip matchesRule n) rs

partOne :: [Ticket] -> [Rule] -> Int
partOne ts rs = sum $ mapMaybe (flip findInvalidNumber rs) ts

reducePossibilities :: Ticket -> [RulePossibilities] -> [RulePossibilities]
reducePossibilities t = map (\(r, ns) -> (r, ns \\ findIndices (not . matchesRule r) t))

-- PRE: mapping length of possibilities over rps produces [1..n] for some n
-- POST: list of (r, [n])
elimination :: [RulePossibilities] -> [RulePossibilities]
elimination rps = go (sortOn (length . snd) rps)
  where 
    go []                   = []
    go (rp@(_, [n]) : rps') = let rps'' = map (\(r, ns) -> (r, delete n ns)) rps'
                              in rp : go rps''

orderRules :: [Rule] -> [Ticket] -> [Rule]
orderRules rs ts = map fst $ sortOn (head . snd) (elimination possiblities)
  where
    possiblities = foldr reducePossibilities initialOrdering ts
    initialOrdering = map (, [0..length rs - 1]) rs

-- PRE: The rules are in the same order as appear on the ticket
partTwo :: [Rule] -> Ticket -> Int
partTwo rs t = product $ mapMaybe (\(n, (f, _)) -> if "departure" `isPrefixOf` f 
                                                   then Just n 
                                                   else Nothing) $ zip t rs

main :: IO ()
main = do 
  contents <- readFile "input"
  (rs, t, ots) <- getInput contents
  print $ partOne ots rs
  let ots' = filter (isNothing . flip findInvalidNumber rs) ots
  print $ partTwo (orderRules rs ots') t