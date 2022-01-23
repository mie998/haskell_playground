mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x * y * z)))

main = do
  print $ mult 1 2 3
