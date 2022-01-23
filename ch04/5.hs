x && y = if x then (if y then True else False) else False

main = do
  print $ True Main.&& True
  print $ True Main.&& False
  print $ False Main.&& True 
  print $ False Main.&& False
