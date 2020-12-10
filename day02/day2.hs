import Text.Regex.PCRE

type Policy = (Int, Int, Char)
type Password = String

data Entry = P Password Policy 

entryRegex :: String 
entryRegex = "^(\\d+)\\-(\\d+) ([a-z]): (.+)$"

-- PRE: All input is valid
entry :: String -> Entry
entry raw = case raw =~ entryRegex :: [[String]] of 
  [[_, lower, upper, [c], pswd]] -> P pswd (read lower, read upper, c)
  _                            -> error "No bad entries allowed"

numOccs :: Eq a => a -> [a] -> Int
numOccs x xs = length $ filter (== x) xs

validPasswordPart1 :: Entry -> Bool
validPasswordPart1 (P pswd (l, u, c)) = l <= occs && occs <= u
  where 
    occs = numOccs c pswd

validPasswordPart2 :: Entry -> Bool
validPasswordPart2 (P pswd (l, u, c)) = first && not second || not first && second
  where 
    first = pswd !! (l - 1) == c
    second = pswd !! (u - 1) == c

getEntries :: String -> IO [Entry]
getEntries filePath = do
  raw <- readFile filePath
  return $ map entry (lines raw)

main :: IO () 
main = do 
  entries <- getEntries "input.txt"
  print $ length $ filter validPasswordPart1 entries
  print $ length $ filter validPasswordPart2 entries
  



