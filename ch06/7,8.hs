
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take l xs, drop l xs)
  where l = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a) (msort b)
  where
    (a, b) = halve xs

main = do
  print(msort [1,3,6,1,8,9])
