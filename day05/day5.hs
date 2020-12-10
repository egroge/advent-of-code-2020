import Data.List
import Data.Maybe

data RowSplit = F | B
data ColumnSplit = L | R 

type BoardingPass = ([RowSplit], [ColumnSplit])
type BoardingPassID = Int

highestRow :: Int
highestRow = 127

-- PRE: length rs == 7
findRow :: BoardingPass -> Int 
findRow b = go (fst b) 0 highestRow
  where 
    -- At this point, l == u + 1
    go [] l u       = u
    go (F : rs) l u = let mid = (l + u) `div` 2 
                      in go rs l mid
    go (B : rs) l u = let mid = (l + u) `div` 2 
                      in go rs mid u

highestColumn :: Int 
highestColumn = 7

-- PRE: length cs == 3
findColumn :: BoardingPass -> Int 
findColumn b = go (snd b) 0 highestColumn
  where 
    go [] l u       = u
    go (L : cs) l u = let mid = (l + u) `div` 2 
                      in go cs l mid
    go (R : cs) l u = let mid = (l + u) `div` 2 
                      in go cs mid u

passID :: BoardingPass -> BoardingPassID 
passID b = 8 * findRow b + findColumn b

rowSplit :: Char -> RowSplit
rowSplit 'F' = F
rowSplit 'B' = B

columnSplit :: Char -> ColumnSplit
columnSplit 'R' = R
columnSplit 'L' = L

pass :: String -> BoardingPass
pass p = (map rowSplit rs, map columnSplit cs)
  where 
    (rs, cs) = splitAt 7 p

getPasses :: String -> IO [BoardingPass]
getPasses = return . map pass . lines

partOne :: [BoardingPass] -> Int 
partOne = maximum . map passID 

partTwo :: [BoardingPass] -> Int 
partTwo bs = snd $ fromJust $ find (uncurry (/=)) $ zip sortedIds [first..]
  where 
    sortedIds@(first : _) = sort $ map passID bs

main :: IO () 
main = do 
  raw    <- readFile "input.txt"
  passes <- getPasses raw
  print (partOne passes)
  print (partTwo passes)

        