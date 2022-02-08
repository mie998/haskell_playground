factor :: Int -> [Int]
factor n = [x | x <- [1..n], mod n x == 0, x /= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factor x)]

main = do
  print(Main.perfects 500)
