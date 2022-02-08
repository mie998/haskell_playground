sqrt :: [Int] -> [Int]
sqrt xs = [x^2 | x <- xs]

main = do
  print(Main.sqrt [1,2,3])
