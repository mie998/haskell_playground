

euclid :: Int -> Int -> Int
euclid 0 m = m  
euclid n 0 = n 
euclid n m = 
  if (mod n m) /= 1
    then Main.euclid m (mod n m)
    else mod n m

main = do
  print(euclid 6 30)
