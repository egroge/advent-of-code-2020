import Data.List.Split (splitOn)
import Data.Map.Strict(Map, empty, (!?), insert, toList)
import Data.List (find, foldl', iterate')
import Data.Maybe (fromJust)
import Control.Monad.State

type Mem = Map Int Turn
type Turn = Int
data GameState = G Turn Mem

instance Show GameState where 
  show (G turn m) = " Turn " ++ show turn ++ " with mem" ++ show (toList m)

getStart :: String -> (Int, GameState)
getStart s = foldl' (\(_, g) n -> step n g) (0, G 1 empty) starting
  where 
    starting = map read (splitOn "," s)

step :: Int -> GameState -> (Int, GameState)
step n (G t m) = case m !? n of 
                   Just last -> (t - last, G (t + 1) (insert n t m))
                   Nothing   -> (0, G (t + 1) (insert n t m))

nth :: Int -> (Int, GameState) -> (Int, GameState)
nth n g = iterate' (uncurry step) g !! n

solution :: Int -> (Int, GameState) -> Int 
solution n x = fst $ fromJust $ find ((n == ) . snd) (toList m)
  where 
    (_, G _ m) = nth n x

