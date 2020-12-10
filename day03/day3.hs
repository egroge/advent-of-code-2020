data Terrain = Tree | Open

type Grid = (Int, Int, [[Terrain]])
type Coord = (Int, Int)
type Slope = (Int, Int)

terr :: Char -> Terrain
terr '.' = Open 
terr '#' = Tree 
terr _   = undefined

constructGrid :: String -> IO Grid 
constructGrid raw = return (rowSize, numRows, grid)
  where 
    grid    = map (map terr) (lines raw)
    numRows = length grid
    rowSize = length (head grid)

move :: Grid -> Coord -> Slope -> Maybe (Coord, Terrain)
move (n, m, g) (x, y) (dx, dy)
  | y' >= m   = Nothing
  | otherwise = Just ((x', y'), t) 
  where
    x' = if x + dx >= n then x + dx - n else x + dx
    y' = y + dy 
    t  = (g !! y') !! x'

checkTrees :: Grid -> Slope -> Int 
checkTrees g s = go (move g (0, 0) s)
  where 
    go Nothing          = 0
    go (Just (c, Tree)) = 1 + go (move g c s)
    go (Just (c, Open)) = go (move g c s)

partOne :: Grid -> Int 
partOne g = checkTrees g (3, 1)

partTwo :: Grid -> Int 
partTwo g = product $ map (checkTrees g) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO () 
main = do 
  contents <- readFile "input.txt"
  grid     <- constructGrid contents
  print (partOne grid)
  print (partTwo grid)



