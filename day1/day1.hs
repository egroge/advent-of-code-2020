
import Data.List

-- PRE there is an answer
answerPartOne :: [Int] -> Int
answerPartOne xs = head [x * y | x <- xs, y <- delete x xs, x + y == 2020]

-- PRE there is an answer
answerPartTwo :: [Int] -> Int
answerPartTwo xs = head [x * y * z | x <- xs, y <- delete x xs, z <- delete x (delete y xs), x + y + z == 2020]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let xs = map read (lines contents)
  print (answerPartOne xs)
  print (answerPartTwo xs)
  