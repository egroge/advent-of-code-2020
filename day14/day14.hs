{-# LANGUAGE TypeApplications #-}

import Text.Parsec hiding ((<|>), empty, parse)
import Control.Applicative ((<|>), empty)
import Data.Functor
import qualified Data.Map as M
import qualified Data.Array as Arr

data Bit = I | O deriving Show
data OptBit = X | B Bit deriving Show 
type Address = Integer -- Not sure what addresses are bounded by if anything 

-- LSB first
type Val = [Bit]
data Assignment = Assign Address Val deriving Show
data Cmd = Update Mask | Write Assignment deriving Show

-- LSB first
type Mask = [OptBit]
type Memory = M.Map Address Integer

-- TODO use record syntax? Since we never update the Mask?
data ProgramState = P Mask [Cmd] Memory

(>?>) :: Parsec String u a -> (a -> Bool) -> Parsec String u a
p >?> f = p >>= (\x -> if f x then pure x else empty)

parseInteger :: Parsec String u Integer 
parseInteger = read @Integer <$> many1 digit

parseBit :: Parsec String u Bit
parseBit = char '1' $> I <|> char '0' $> O

parseOptBit :: Parsec String u OptBit
parseOptBit = char 'X' $> X <|> (B <$> parseBit)

parseMask :: Parsec String u Mask
parseMask = string "mask = " *> (reverse <$> (many parseOptBit >?> ((36 == ) . length)))

parseVal :: Parsec String u Val 
parseVal = decTo36Bin <$> parseInteger

parseAssignment :: Parsec String u Assignment
parseAssignment = Assign <$> (string "mem[" *> parseInteger) <*> (string "] = " *> parseVal)

parseCmd :: Parsec String u Cmd
parseCmd = (Update <$> try parseMask) <|> (Write <$> parseAssignment)

parse :: Parsec String () a -> String -> a
parse p input = either (const undefined) id $ runParser p () "" input 

getStartState :: String -> IO ProgramState
getStartState raw = return $ case lines raw of 
                               (maskRaw : rem) -> P (parse parseMask maskRaw) (map (parse parseCmd) rem) M.empty

-- POST: LSB first
decTo36Bin :: Integer -> [Bit]
decTo36Bin n = take 36 (decToBin' n ++ repeat O)
  where 
    remToBit  r = if r == 1 then I else O
    decToBin' 0 = []
    decToBin' n = let (q, r) = quotRem n 2 
                  in remToBit r : decToBin' q

-- PRE: LSB first
binToDec :: [Bit] -> Integer
binToDec bs = binToDec' bs 1
  where 
    binToDec' [] _        = 0
    binToDec' (O : bs') n = binToDec' bs' (2 * n)
    binToDec' (I : bs') n = n + binToDec' bs' (2 * n)

-- PRE: LSB first
mask1 :: [Bit] -> Mask -> [Bit]
mask1 = zipWith andBit 
  where 
    andBit b X     = b
    andBit _ (B b) = b

step1 :: ProgramState -> ProgramState
step1 (P msk ((Write (Assign addr val)) : as) mem) 
  = P msk as (M.insert addr (binToDec (mask1 val msk)) mem)
step1 (P _ ((Update msk) : as) mem) 
  = P msk as mem

-- NOTE: This is infeasible on any mask with too many Xs. Cannot think of a way to avoid that.
-- For example, the mask in the sample file is too much for it
mask2 :: [Bit] -> Mask -> [[Bit]]
mask2 [] _                  = [[]]
mask2 (b : bs) ((B O) : ms) = map (b :) $ mask2 bs ms 
mask2 (_ : bs) ((B I) : ms) = map (I :) $ mask2 bs ms
mask2 (_ : bs) (X : ms)     = map (I :) rem ++ map (O :) rem
  where 
    rem = mask2 bs ms

updateMemory :: [Assignment] -> Memory -> Memory
updateMemory as m = foldr write m as
  where 
    write (Assign addr val) = M.insert addr (binToDec val)

step2 :: ProgramState -> ProgramState
step2 (P msk ((Write (Assign addr val)) : as) mem) 
  = P msk as (updateMemory assigns mem)
  where 
    assigns = map (\a -> Assign (binToDec a) val) (mask2 (decTo36Bin addr) msk)
step2 (P _ ((Update msk) : as) mem) 
  = P msk as mem

progFinished :: ProgramState -> Bool 
progFinished (P _ as _) = null as

memory :: ProgramState -> Memory
memory (P _ _ mem) = mem

runProg :: (ProgramState -> ProgramState) -> ProgramState -> Memory 
runProg stepper p = memory $ until progFinished stepper p

-- 13105044880745
partOne :: ProgramState -> Integer 
partOne p = sum $ M.elems $ runProg step1 p

-- 3505392154485
partTwo :: ProgramState -> Integer 
partTwo p = sum $ M.elems $ runProg step2 p

main :: IO () 
main = do 
  raw   <- readFile "input"
  start <- getStartState raw
  print $ partOne start
  print $ partTwo start