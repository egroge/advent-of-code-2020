{-# LANGUAGE TypeApplications #-}

import Text.Parsec hiding ((<|>), empty, many)
import Control.Applicative
import Data.Functor

data Term = Num Int | Add Term | Mul Term | Sub Expr deriving Show
type Expr = [Term]

infixl 3 <\>
(<\>) :: Parsec s u a -> Parsec s u a -> Parsec s u a
p <\> p' = try p <|> p'

num :: Parsec String u Term 
num = Num . read @Int <$> many1 digit

infixOp :: Parsec String u (Term -> Term)
infixOp = (string " + " $> Add) <\> (string " * " $> Mul) 

subExpr :: Parsec String u Term 
subExpr = char '(' *> (Sub <$> expr) <* char ')' 

term :: Parsec String u Term
term = subExpr <\> num <\> (infixOp <*> term) 

expr :: Parsec String u Expr
expr = many1 term

exprs :: Parsec String u [Expr]
exprs = try expr `sepBy` char '\n'

getExprs :: String -> IO [Expr]
getExprs = return . either (error . show) id . runParser exprs () "" 

-- eval :: Expr -> Int 
-- eval e = go Nothing e
--   where 
--     go Nothing (Num n)  = n
--     go (Just n) (Add e) = n + go 
 

main :: IO () 
main = do 
  contents <- readFile "sample"
  print contents
  es       <- getExprs contents 
  mapM_ print es

