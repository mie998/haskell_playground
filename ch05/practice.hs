import Distribution.Simple (UserHooks(postInst))
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Integral a => a -> [a] -> [Int]
positions n xs = [n' | (x,n') <- zip xs [0..], n == x]

main = do
  print(pairs [1,2,3,4])
