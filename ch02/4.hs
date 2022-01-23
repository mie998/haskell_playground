myLast1 xs = head (reverse xs)

myLast2 [] = error "empty list"
myLast2 [x] = x
myLast2 (x:xs) = myLast2 xs

myLast3 [] = error "empty list"
myLast3 [x] = x
myLast3 xs = myLast3 (drop 1 xs)

main = do
  print $ last [1,2,3,4,5] 
  print $ myLast1 [1,2,3,4,5] 
  print $ myLast2 [1,2,3,4,5] 
  print $ myLast3 [1,2,3,4,5] 

