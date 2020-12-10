import Data.List
import Data.Maybe

-- PRE length pream == preambleSize
isValid :: [Int] -> Int-> Bool
isValid pream n = not $ null [(x, y) | x <- pream, y <- delete x pream, n == x + y]

step :: [Int] -> ([Int], Bool)
step ns
  | length ns < preambleSize = ([], True)
  | valid                    = (tail ns, True)
  | otherwise                = (drop preambleSize ns, False)
  where
    valid = isValid (take preambleSize ns) (ns !! preambleSize)

findInvalid :: [Int] -> Maybe Int 
findInvalid ns = case finalState of 
                   ([], _)      -> Nothing
                   (ns', False) -> Just (head ns')
  where
    finalState       = until finished (step . fst) (ns, True)
    finished (xs, b) = null xs || not b

findSectionSummingTo :: [Int] -> Int -> Maybe [Int]
findSectionSummingTo ns target = fromJust <$> find isJust (map (go 0) (tails ns))
  where 
    go s [] = if s == target then Just [] else Nothing 
    go s (n : ns)
      | n + s == target     = Just [n]
      | n + s > target      = Nothing
      | otherwise           = (n :) <$> go (n + s) ns

partOne :: [Int] -> Int 
partOne = fromJust . findInvalid

partTwo :: [Int] -> Int -> Int 
partTwo ns invalid = minimum section + maximum section
  where 
    section = fromJust $ findSectionSummingTo ns invalid

preambleSize :: Int
preambleSize = 25 

main :: IO () 
main = do 
  contents <- readFile "input"
  let ns = map read (lines contents) :: [Int]
  let invalid = partOne ns
  print invalid
  -- print $ findSectionSummingTo ns invalid
  print $ partTwo ns invalid