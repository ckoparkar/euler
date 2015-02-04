-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?

import Data.List.Split

loadWords :: IO [String]
loadWords = do
  allWords <- readFile "res/p042_words.txt"
  let quotedWords = splitOn "," allWords
  return $ map (filter (/= '"')) quotedWords

wordValue [] = 0
wordValue (x:xs) = charValue x + wordValue xs
  where charValue c = fromIntegral $ fromEnum c - fromEnum 'A' + 1

triangles :: [Integer]
triangles = triangles' 1
  where triangles' x = t x : (triangles' (x+1))
        t x = round $ 0.5 * x * (x+1)

m = do
  ws <- loadWords
  let vs = map wordValue ws
  return $ length $ filter (\x -> x == (last (takeWhile (<= x) triangles))) vs
