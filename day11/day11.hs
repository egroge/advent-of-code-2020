import Data.Maybe
import Data.List 

-- Occupied, unoccupied, floor
data Space = Occ | UnOcc | F deriving Eq

instance Show Space where 
  show Occ   = "#"
  show UnOcc = "L"
  show F     = "."

-- NOTE: This is very slow. End of pintos today, so cba to make it faster, but would do this by 
-- using Array Coord Space instead of a [[Space]] for the grid.

type Grid = [[Space]]
type Room = (Int, Int, Grid)
type Coord = (Int, Int)

roomStr :: Room -> String
roomStr (_, _, g) = ('\n' :) $ intercalate "\n" $ map (concatMap show) g

space :: Char -> Space 
space '#' = Occ
space 'L' = UnOcc
space '.' = F

occ :: Space -> Bool
occ Occ = True 
occ _   = False

getRoom :: String -> IO Room 
getRoom raw = return (width, height, grid)
  where 
    height = length grid
    width  = length (head grid)
    grid   = map (map space) (lines raw)

numAdjacentOccupied :: Coord -> Room -> Int
numAdjacentOccupied (x, y) (w, h, g) = sum $ map (\(dx, dy) -> seatVal (x + dx) (y + dy)) surrounding
  where
    surrounding = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (0, -1), (1, -1), (1, 0), (1, 1)]
    seatVal x' y'
      | x' < 0  || y' < 0  = 0
      | x' == w || y' == h = 0
      | otherwise          = case (g !! y') !! x' of 
                               Occ -> 1
                               _   -> 0

rows :: Room -> [[Space]]
rows (_, _, g) = g 

cols :: Room -> [[Space]]
cols = transpose . rows

inBounds :: Coord -> Room -> Bool 
inBounds (x, y) (w, h, _) = x >= 0 && x < w && y >= 0 && y < h

-- This may be some of the grossest haskell I hath seen
numVisibleSeats :: Coord -> Room -> Int
numVisibleSeats (x, y) room@(w, h, g) = sum $ map visibleScore [reverse r, r', reverse c, c', d1, d2, d3, d4]
  where 
    visibleScore []          = 0
    visibleScore (Occ : _)   = 1
    visibleScore (UnOcc : _) = 0
    visibleScore (F : ss)    = visibleScore ss

    (rs, cs) = (rows room, cols room)
    (r, r')  = (take x (rs !! y), drop (x + 1) (rs !! y))
    (c, c')  = (take y (cs !! x), drop (y + 1) (cs !! x))
    [d1, d2, d3, d4] = map (uncurry (diag x y)) [(-1, -1), (-1, 1), (1, -1), (1, 1)]

    diag :: Int -> Int -> Int -> Int -> [Space]
    diag x y dx dy 
      | not $ inBounds (x', y') room = []
      | otherwise                    = ((g !! y') !! x') : diag x' y' dx dy
      where 
        (x', y') = (x + dx, y + dy)

step :: (Coord -> Room -> Int) -> Int -> Room -> Room
step f threshold r@(w, h, g) = (w, h, map (map (uncurry transformSeat)) coords)
  where 
    seat x y = (g !! y) !! x
    coords   = [[(x, y) | x <- [0..w - 1]] | y <- [0..h - 1]]
    transformSeat x y = case seat x y of 
                          F     -> F
                          UnOcc -> if f (x, y) r == 0 then Occ else UnOcc
                          Occ   -> if f (x, y) r >= threshold then UnOcc else Occ 


count :: (a -> Bool) -> [a] -> Int
count = (length . ) . filter

solution :: (Coord -> Room -> Int) -> Int -> Room -> Int
solution f threshold r = if r == r' then countOcc r' else solution f threshold r'
  where 
    r' = step f threshold r 
    countOcc (_, _, g) = count occ (concat g)

-- 2453, 2159
main :: IO () 
main = do 
  raw  <- readFile "input"
  room <- getRoom raw
  print $ solution numAdjacentOccupied 4 room 
  print $ solution numVisibleSeats 5 room 
