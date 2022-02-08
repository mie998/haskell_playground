length :: [a] -> Int
length xs = sum [1 | _ <- xs]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [0..n]] 

main = do
  print(Main.replicate 3 True)
