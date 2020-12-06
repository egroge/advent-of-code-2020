import Data.List.Split (splitOn)
import Data.List (nub, intersect)

type GroupAnswer = [String] 

groupAnswers :: String -> GroupAnswer
groupAnswers = lines

getAnswers :: String -> [GroupAnswer]
getAnswers = map groupAnswers . splitOn "\n\n" 

-- 7110
partOne :: [GroupAnswer] -> Int 
partOne = sum . map (length . nub . concat)

partTwo :: [GroupAnswer] -> Int 
partTwo = length . concatMap (foldr1 intersect) 

main :: IO () 
main = do 
  raw <- readFile "input.txt"
  let answers = getAnswers raw
  print $ partOne answers
  print $ partTwo answers



