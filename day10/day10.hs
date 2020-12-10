import Data.List (sort)
import Data.Map (Map, (!?), empty, insert, singleton, union, size)
import Data.Maybe
import Control.Monad.State

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
combs js = evalState (combs' js) empty
  where
    combs' :: [Int] -> State (Map [Int] Int) Int
    combs' js@(j : _ : j' : js') = do 
        m <- get
        case m !? js of 
          Just n  -> return n 
          Nothing -> do
            nKept <- combs' (tail js)
           
            if j' - j <= 3 
            then do 
              nDiscarded <- combs' (j : j' : js')
              let both = nDiscarded + nKept
              m' <- get 
              put (insert js both m')
              return both
            else do 
              m' <- get
              put (insert js nKept m')
              return nKept 
    combs' js = do
      m <- get 
      case m !? js of 
        Just n  -> return n 
        Nothing -> put (singleton js 1) >> return 1

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
  