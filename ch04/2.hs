myThird1 xs = head (tail (tail xs))
myThird2 xs = xs!!2
myThird3 [_,_,x,_] = x
            

main = do
  print $ myThird1 [1,2,3,4]
  print $ myThird2 [1,2,3,4]
  print $ myThird3 [1,2,3,4]
