luhnDouble :: Int -> Int
luhnDouble n | n * 2 > 9 = n * 2 - 9
             | otherwise = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0

main = do
  print $ luhnDouble 3
  print $ luhnDouble 6
  print $ luhn 1 7 8 4
  print $ luhn 4 7 8 3

