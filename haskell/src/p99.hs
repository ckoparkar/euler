-- Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would confirm that 211 = 2048 < 37 = 2187.

-- However, confirming that 632382518061 > 519432525806 would be much more difficult, as both numbers contain over three million digits.

-- Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.

-- NOTE: The first two lines in the file represent the numbers in the example given above.

-- Trick: log(a^x) = x * log a
--        logb > loga if a > b.

-- Running time: 0.02 secs

import Data.List.Split

loadNumberPairs :: String -> IO [([Integer], Integer)]
loadNumberPairs path = do
  allLines <- readFile path
  let ps = splitOn "\n" allLines                        -- ["1,1", "2,2"]
      ts = map (splitOn ",") ps                         -- [["1,1"], ["2,2"]]
      ns = map (\t -> map (\x -> read x :: Integer) t) ts  -- [[1,1], [2,2]]
      os = zip ns [1..]
  return os

m :: IO Integer
m = do
  ws <- loadNumberPairs "res/p099_base_exp.txt"
  let xs = map (\(x, y) -> (fromIntegral (x!!1) * log(fromIntegral (x!!0)), y)) ws
  return $ snd $ maximum xs
