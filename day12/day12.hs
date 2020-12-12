{-# LANGUAGE FlexibleInstances #-}

type Angle = Float

data Instr = F Float | N Float | E Float | S Float | W Float | R Angle | L Angle deriving Show 

type Coord = (Float, Float)
type ShipState1 = (Coord, Angle)
type ShipState2 = (Coord, Coord)

advance1 :: Instr -> ShipState1 -> ShipState1
advance1 (N n) ((x, y), a) = ((x, y + n), a)
advance1 (E n) ((x, y), a) = ((x + n, y), a)
advance1 (S n) ((x, y), a) = ((x, y - n), a)
advance1 (W n) ((x, y), a) = ((x - n, y), a)
advance1 (L n) (c, a)      = (c, a + n)
advance1 (R n) (c, a)      = (c, a - n)
advance1 (F n) ((x, y), a) = let a' = degreeToRadian a 
                             in  ((x + n * cos a', y + n * sin a'), a)

advance2 :: Instr -> ShipState2 -> ShipState2
advance2 (N n) (c, (wx, wy))        = (c, (wx, wy + n))
advance2 (E n) (c, (wx, wy))        = (c, (wx + n, wy))
advance2 (S n) (c, (wx, wy))        = (c, (wx, wy - n))
advance2 (W n) (c, (wx, wy))        = (c, (wx - n, wy))
advance2 (R a) (c, w)               = (c, rotateCoord w (-a))
advance2 (L a) (c, w)               = (c, rotateCoord w a)
advance2 (F n) ((x, y), w@(wx, wy)) = ((x + n * wx, y + n * wy), w)

class ShipState a where 
  start   :: a
  coord   :: a -> Coord
  advance :: Instr -> a -> a
  travel  :: [Instr] -> a -> a 
  travel ins s = foldl (flip advance) s ins

instance ShipState ShipState1 where 
  start        = ((0, 0), 0)
  coord (c, _) = c 
  advance      = advance1

instance ShipState ShipState2 where 
  start        = ((0, 0), (10, 1))
  coord (c, _) = c 
  advance      = advance2

getInstr :: String -> Instr 
getInstr (c : cs) 
  = let ins = case c of 
                'F' -> F
                'N' -> N 
                'E' -> E 
                'S' -> S 
                'W' -> W 
                'R' -> R 
                'L' -> L
    in ins (read cs)

degreeToRadian :: Float -> Float
degreeToRadian n = n * (pi / 180)

rotateCoord :: Coord -> Angle -> Coord
rotateCoord (x, y) a 
  = let a' = degreeToRadian a
        x' = x * cos a' - y * sin a'
        y' = x * sin a' + y * cos a'
    in  (x', y') 

manhattanDist :: ShipState a => a -> a -> Float 
manhattanDist s s' = let (x, y)   = coord s 
                         (x', y') = coord s'
                     in  abs (x - x') + abs (y - y')

-- 2879.0005 or 2878.999
-- 178986.0
main :: IO () 
main = do 
  raw          <- readFile "input"
  let ins       = map getInstr (lines raw)
  let endState1 = travel ins start 
  print $ manhattanDist (start :: ShipState1) endState1
  let endState2 = travel ins start 
  print $ manhattanDist (start :: ShipState2) endState2
  