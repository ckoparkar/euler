mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort f [] = []
mergeSort f [x] = [x]
mergeSort f xs = merge f (mergeSort f as) (mergeSort f bs)
  where (as, bs) = splitAt (length xs `div` 2) xs

merge :: (t -> t -> Bool) -> [t] -> [t] -> [t]
merge f as [] = as
merge f [] bs = bs
merge f (a:as) (b:bs)
  | f a b = a : merge f as (b:bs)
  | otherwise = b : merge f (a:as) bs

m = mergeSort (<) [100,99..1]
n = mergeSort (>) [1..100]
o = mergeSort (\a b -> length a < length b) ["aaaa","aaa","aa","a"]
p = mergeSort (\a b -> length a < length b) [[4444], [333],[22], [1]]
q = mergeSort (\a b -> sum a < sum b) [[1111], [222],[33], [4]]
