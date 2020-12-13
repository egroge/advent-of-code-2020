import Data.List.Split (splitOn)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad.State

type Timestamp = Int
type BusID = Int

earliestTime :: String -> Timestamp 
earliestTime = read 

busIdsPos :: String -> [(Timestamp, Maybe BusID)]
busIdsPos s = zip [0..] $ map readMaybe (splitOn "," s)

findBestBusTime :: Timestamp -> BusID -> Timestamp
findBestBusTime t bid 
  | r == 0    = t 
  | otherwise = (q + 1) * bid
  where 
    (q, r) = quotRem t bid

findBestBusTimes :: Timestamp -> [BusID] -> [(BusID, Timestamp)]
findBestBusTimes t = map (\b -> (b, findBestBusTime t b)) 

partOne :: Timestamp -> [BusID] -> Int 
partOne t bids = bid * (t' - t)
  where 
    (bid, t') = minimumBy (comparing snd) $ findBestBusTimes t bids

partTwo :: [(Timestamp, Maybe BusID)] -> Timestamp 
partTwo ((0, Just bid) : xs') = evalState go (bid, 0, xs')
  where 
    go :: State (Timestamp, Timestamp, [(Timestamp, Maybe BusID)]) Timestamp
    go = do 
      (j, t, xs) <- get
      case xs of 
        []         -> return t
        (x' : xs') -> case x' of 
          (_, Nothing)    -> put (j, t, xs') >> go 
          (pos, Just bid) -> if (t + pos) `mod` bid == 0
                             then put (j * bid, t, xs') >> go 
                             else put (j, t + j, xs)  >> go 

-- 2165
main :: IO () 
main = do 
  contents <- readFile "input"
  let [tRaw, bidsRaw] = lines contents
  let t = earliestTime tRaw
  let bidsPos = busIdsPos bidsRaw
  print $ partOne t (mapMaybe snd bidsPos)
  print $ partTwo bidsPos
