module Utils where

removeEndSpace :: String -> String
removeEndSpace str = reverse (dropWhile (== ' ')  (reverse str))

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

-- chat GPT generated
splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy s l =
  first : splitBy s ( if null remain then remain else dropWhile (== ' ') (tail remain))
  where
    first = firstValid l 0
    firstValid [] _ = []
    firstValid (x : xs) c
      | x == s && c == 0 = []
      | x == '(' = x : firstValid xs (c + 1)
      | x == ')' = x : firstValid xs (c - 1)
      | otherwise = x : firstValid xs c
    opening = length (filter (== '(') first)
    closing = length (filter (== ')') first)
    remain = drop (length first) l

uniques :: Eq a => [a] -> [a]
uniques [] = [] 
uniques (x:xs) = x : uniques (filter (/= x) xs)