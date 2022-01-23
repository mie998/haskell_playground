
myInit1 xs = take (length xs - 1) xs 
myInit2 xs = reverse (tail (reverse xs))

main = do
  print $ myInit1 [1,2,3,4,5] 
  print $ myInit2 [1,2,3,4,5] 
