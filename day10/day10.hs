import Data.List (sort)
import Data.Map (Map, (!?), empty, insert, singleton, union, size)
import Data.Maybe

type Jolt = Int 

diffs :: [Jolt] -> [Int]
diffs js = let js' = 0 : sort js
           in zipWith (-) (tail js') js' ++ [3]

partOne :: [Jolt] -> Int 
partOne js = length (filter (== 1) ds) * length (filter (== 3) ds)
  where 
    ds = diffs js

getJolts :: String -> IO [Jolt]
getJolts = return . map read . lines

-- PRE: js is sorted and includes starting 0 and final max + 3
combs :: [Jolt] -> Int
combs js = fst (go js empty)
  where 
    -- TODO this is too messy
    go :: [Int] -> Map [Int] Int -> (Int, Map [Int] Int)
    go js@(j : j' : j'' : js') m
      | diff <= 3 && diff >= 1 = (combined, insert js combined mCombined)
      | otherwise              = (nKept, insert js nKept mKept)
      where
        diff = j'' - j
        combined    = nKept + nDiscarded
        mCombined   = mKept `union` mDiscarded
        keptJs      = j' : j'' : js'
        discardedJs = j : j'' : js'

        (nKept, keptMap)           = case m !? keptJs of 
                                      Just n  -> (n, m)
                                      Nothing -> go keptJs m

        (nDiscarded, discardedMap) = case keptMap !? discardedJs of 
                                      Just n  -> (n, m)
                                      Nothing -> go discardedJs keptMap 
    go js m = case m !? js of
                Just n  -> (n, m)
                Nothing -> (1, singleton js 1)

partTwo :: [Jolt] -> Int 
partTwo js = combs longest
  where 
    longest  = 0 : sortedJs ++ [last sortedJs + 3]
    sortedJs = sort js

main :: IO () 
main = do 
  raw <- readFile "input"
  js  <- getJolts raw
  print $ partOne js
  print $ partTwo js
  