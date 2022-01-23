halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

main = do
  print $ halve [1,3,3,4,5]
