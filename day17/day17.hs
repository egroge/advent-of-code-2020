import Data.Set (Set, union, member)
import qualified Data.Set as S

data Coord3 = C3 Int Int Int deriving (Eq, Ord, Show)
data Coord4 = C4 Int Int Int Int deriving (Eq, Ord, Show)

-- Given slice is z = 0
getInitialState :: String -> (Set Coord3, Set Coord4)
getInitialState raw = go raw 0 0 S.empty S.empty
  where 
    go ('#'  : raw') x y s3 s4 = go raw' (x + 1) y (S.insert (C3 x y 0) s3) (S.insert (C4 x y 0 0) s4)
    go ('.'  : raw') x y s3 s4 = go raw' (x + 1) y s3 s4
    go ('\n' : raw') _ y s3 s4 = go raw' 0 (y + 1) s3 s4
    go "" _ _ s3 s4            = (s3, s4)

neighbours3 :: Coord3 -> [Coord3]
neighbours3 (C3 x y z) = let diffs = [-1, 0, 1]
                         in [C3 (x + dx) (y + dy) (z + dz) | 
                            dx <- diffs, dy <- diffs, dz <- diffs, dx /= 0 || dy /= 0 || dz /= 0]

neighbours4 :: Coord4 -> [Coord4]
neighbours4 (C4 x y z w) = [C4 x' y' z' w | (C3 x' y' z') <- neighbours3 (C3 x y z), dw <- [-1, 0, 1]]
 
step :: Ord a => (a -> [a]) -> Set a -> Set a
step neighbours s = S.filter inactiveToActive inactive `union` S.filter activeStaysActive active
  where 
    inactiveToActive c    = numActiveNeighbours c == 3
    activeStaysActive c   = let n = numActiveNeighbours c in n == 2 || n == 3
    (active, inactive)    = S.partition (`member` s) considered
    numActiveNeighbours c = length $ filter (`member` s) (neighbours c)
    considered            = foldr S.insert s (concatMap neighbours (S.toList s))

partOne :: Set Coord3 -> Int 
partOne s = S.size $ iterate (step neighbours3) s !! 6

partTwo :: Set Coord4 -> Int 
partTwo s = S.size $ iterate (step neighbours4) s !! 6

main :: IO () 
main = do 
  raw  <- readFile "input"
  let (s3, s4) = getInitialState raw 
  print $ partOne s3
  print $ partTwo s4