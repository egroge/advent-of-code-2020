{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Text.Parsec hiding ((<|>), empty, many)
import Control.Applicative
import Data.Functor
import Data.String

data Expr = Num Int | Add Expr Expr | Mul Expr Expr | Sub Expr deriving Show

instance (s ~ String) => IsString (Parsec String u s) where 
  fromString = lexeme . string

whitespace :: Parsec String u String 
whitespace = many (char ' ')

lexeme :: Parsec String u a -> Parsec String u a 
lexeme p = p <* whitespace

num :: Parsec String u Expr 
num = lexeme $ Num . read @Int <$> many1 digit

add :: Parsec String u (Expr -> Expr -> Expr)
add = "+" $> Add

mul :: Parsec String u (Expr -> Expr -> Expr)
mul = "*" $> Mul

subExpr :: Parsec String u Expr -> Parsec String u Expr 
subExpr expr = "(" *> (Sub <$> expr) <* ")"

exprAddMul :: Parsec String u Expr
exprAddMul = chainl1 term mul 
  where 
    atom = num <|> subExpr exprAddMul
    term = chainl1 atom add

exprNoPrecendence :: Parsec String u Expr
exprNoPrecendence = chainl1 atom (add <|> mul)
  where 
    atom = num <|> subExpr exprNoPrecendence

exprs :: Parsec String u Expr -> Parsec String u [Expr]
exprs p = p `sepBy` char '\n'

getExprs :: String -> Parsec String () Expr -> IO [Expr]
getExprs s e = return $ either (error . show) id $ runParser (exprs e) () "" s

eval :: Expr -> Int 
eval (Num n) = n
eval (Add e e') = eval e + eval e'
eval (Mul e e') = eval e * eval e'
eval (Sub e)    = eval e

solution :: [Expr] -> Int 
solution = sum . map eval

main :: IO () 
main = do 
  contents <- readFile "input"
  esNoPrec <- getExprs contents exprNoPrecendence
  esPrec   <- getExprs contents exprAddMul
  print $ solution esNoPrec
  print $ solution esPrec


