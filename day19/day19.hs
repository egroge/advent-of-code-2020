{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Text.Parsec 
import Data.String
import Data.Maybe (fromJust)
import qualified Data.Map as M

instance (s ~ String) => IsString (Parsec String u s) where 
  fromString = lexeme . string

type RuleId = Int
data Rule = Composed [RuleId] | CharRule Char | AnyOf [Rule] deriving Show

type Rules = M.Map RuleId Rule

chainPre :: Parsec s u (a -> a) -> Parsec s u a -> Parsec s u a
chainPre op p = op <*> chainPre op p <|> p

pfoldr :: (a -> b -> b) -> b -> Parsec s u a -> Parsec s u b
pfoldr f k p = chainPre (f <$> p) (pure k) 

(<~>) :: Parsec s u a -> Parsec s u b -> Parsec s u (a, b)
p <~> p' = (,) <$> p <*> p'

whitespace :: Parsec String u String 
whitespace = many (char ' ')

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* whitespace 

ruleId :: Parsec String u Int 
ruleId = lexeme (read <$> many1 digit)

newRuleId :: Parsec String u RuleId
newRuleId = ruleId <* ":"

charRule :: Parsec String u Rule 
charRule = "\"" *> (CharRule <$> anyChar) <* "\""

anyOfRule :: Parsec String u Rule 
anyOfRule = f <$> (composedRule `sepBy1` "|")
  where 
    composedRule = Composed <$> (ruleId `sepBy1` whitespace)
    f [r] = r 
    f rs  = AnyOf rs

rule :: Parsec String u Rule 
rule = (anyOfRule <|> charRule) <* "\n"

rules :: Parsec String u Rules 
rules = pfoldr (uncurry M.insert) M.empty (newRuleId <~> rule)

getRulesAndCases :: String -> IO (Rules, [String])
getRulesAndCases = return . either (error . show) id . runParser p () ""
  where 
    p = whitespace *> ((rules <* "\n") <~> (many (noneOf "\n") `sepBy` "\n"))

matchesRule' :: Rule -> Rules -> String -> [(Bool, String)]
matchesRule' (CharRule _) _ ""           = [(False, "")]
matchesRule' (CharRule c) _ ss@(s : ss') = [if c == s then (True, ss') else (False, ss)]
matchesRule' (AnyOf cs) rs s             = concatMap (\c -> matchesRule' c rs s) cs
matchesRule' (Composed rIds) rs ss       = go rIds ss 
  where 
    go [] s = [(True, s)]
    go (rId : rIds') s = case filter fst (matchesRule' (fromJust (M.lookup rId rs)) rs s) of 
                           [] -> [(False, ss)]
                           xs -> concatMap (go rIds' . snd) xs

matchesRule :: Rule -> Rules -> String -> Bool
matchesRule r rs s = any (\(b, s') -> b && null s') (matchesRule' r rs s)

solution :: Rules -> [String] -> Int 
solution rs ss = length $ filter (matchesRule (fromJust $ M.lookup 0 rs) rs) ss

updateRules :: Rules -> Rules
updateRules rs = foldr (uncurry M.insert) rs [new11, new8]
  where 
    new8  = (8, AnyOf [Composed [42], Composed [42, 8]])
    new11 = (11, AnyOf [Composed [42, 31], Composed [42, 11, 31]])

-- 184, 389
main :: IO () 
main = do 
  contents <- readFile "input" 
  (rs, ss) <- getRulesAndCases contents 
  print $ solution rs ss 
  let rs' = updateRules rs
  print $ solution rs' ss 
