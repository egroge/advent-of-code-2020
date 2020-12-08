{-# LANGUAGE TypeApplications #-}

import Text.Parsec hiding (label, (<|>))
import Data.Functor
import Data.Array hiding (accum)
import Data.Array.Unsafe
import Data.Array.ST 
import Data.Set hiding (elems)
import Control.Applicative ((<|>))
import Data.Maybe
import Control.Monad.ST

data Instr = Nop Int | Acc Int | Jmp Int deriving (Eq, Show)

type Label = Int
type PC  = Label 
type Accum = Int

data Context = C (Array Label Instr) PC Accum

label :: Context -> Label 
label (C _ l _) = l

accum :: Context -> Accum 
accum (C _ _ a) = a

infixl 3 <\>
(<\>) :: Parsec s u a -> Parsec s u a -> Parsec s u a 
p <\> p' = try p <|> p'

(<:>) :: Parsec s u a -> Parsec s u [a] -> Parsec s u [a] 
p <:> p' = (:) <$> p <*> p'

nop :: Parsec String u (Int -> Instr)
nop = string "nop " $> Nop

acc :: Parsec String u (Int -> Instr)
acc = string "acc " $> Acc

jmp :: Parsec String u (Int -> Instr)
jmp = string "jmp " $> Jmp

operand :: Parsec String u Int
-- TODO would prefer char '-' to be optional or something
operand = read @Int <$> (char '-' <:> many1 digit <\> char '+' *> many1 digit)

cmd :: Parsec String u Instr
cmd = (nop <\> acc <\> jmp) <*> operand

cmds :: Parsec String u [Instr]
cmds = (cmd `sepBy` char '\n') <* eof

getProgram :: String -> Array Label Instr
getProgram s = array (0, length instrs - 1) instrs
  where 
    instrs = zip [0..] $ either (const []) id $ runParser cmds () "" s

step :: Context -> Maybe Context 
step (C prog pc acc) 
  | inRange (bounds prog) pc = Just next
  | otherwise                = Nothing
  where 
    next = case prog ! pc of 
             Nop _ -> C prog (pc + 1) acc
             Jmp j -> C prog (pc + j) acc
             Acc a -> C prog (pc + 1) (acc + a)

-- Returns final context and true, or context when repeating and false
runUntilLoopingOrDone :: Context -> (Context, Bool)
runUntilLoopingOrDone c = go c empty
  where 
    go :: Context -> Set Label -> (Context, Bool)
    go c ls 
      | label c `member` ls = (c, False)
      | otherwise = case step c of 
                      Just c' -> go c' (insert (label c) ls)
                      Nothing -> (c, True)

-- PRE: The program definitely loops
partOne :: Context -> Int
partOne c = accum $ fst $ runUntilLoopingOrDone c

-- PRE: snd (runUntilLoopingOrDone c) is False
fixLoopingMachine :: Context -> Context
fixLoopingMachine (C prog start startAcc) = C fixed start startAcc
  where
    fixed :: Array Label Instr
    fixed = runSTArray $ findFix 0 

    findFix :: Label -> ST s (STArray s Label Instr)
    findFix l = do 
      -- Does this progMut get computed every time? Is that expensive?
      progMut <- unsafeThaw prog
      let terminates arr = runUntilLoopingOrDone (C arr start startAcc)
      (_, isFix) <- testFix progMut l (terminates <$> unsafeFreeze progMut)
      
      if isFix then do 
        oldInstr <- readArray progMut l 
        writeArray progMut l (changeElem oldInstr)
        return progMut
      else findFix (l + 1)

    testFix :: STArray s Label Instr -> Label -> ST s b -> ST s b 
    testFix arr l action = do
      oldInstr <- readArray arr l 
      writeArray arr l (changeElem oldInstr)
      y <- action
      writeArray arr l oldInstr
      return y
        
    changeElem (Nop i) = Jmp i
    changeElem (Jmp i) = Nop i
    changeElem other   = other     

partTwo :: Context -> Int
partTwo c = accum $ fst $ runUntilLoopingOrDone fixed
  where 
    fixed = fixLoopingMachine c

main :: IO () 
main = do 
  contents <- readFile "input"
  let program = getProgram contents
  let con     = C program 0 0
  print $ partOne con
  print $ partTwo con
