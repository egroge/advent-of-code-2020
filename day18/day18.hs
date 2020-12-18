{-# LANGUAGE TypeApplications #-}

import Text.Parsec hiding ((<|>), empty, many)
import Control.Applicative
import Data.Functor

data Expr = Num Int | Add Expr Expr | Mul Expr Expr | Sub Expr deriving Show

num :: Parsec String u Expr 
num = Num . read @Int <$> many1 digit

add :: Parsec String u (Expr -> Expr -> Expr)
add = string " + " $> Add

mul :: Parsec String u (Expr -> Expr -> Expr)
mul = string " * " $> Mul

subExpr :: Parsec String u Expr -> Parsec String u Expr 
subExpr expr = char '(' *> (Sub <$> expr) <* char ')' 

exprAddMul :: Parsec String u Expr
exprAddMul = chainl1 term (try mul) 
  where 
    atom = num <|> subExpr exprNoPrecendence
    term = chainl1 atom (try add)

exprNoPrecendence :: Parsec String u Expr
exprNoPrecendence = chainl1 atom (try add <|> mul)
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


