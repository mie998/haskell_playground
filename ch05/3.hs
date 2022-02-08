square :: Int -> [(Int, Int)]
square n = [(x1, x2) | x1 <- [0..n], x2 <- [0..n], x1 /= x2]

main = do
  print(square 2)
