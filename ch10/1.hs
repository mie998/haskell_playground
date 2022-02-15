myPutStr :: String -> IO()
myPutStr s = do
  sequence_ [putChar c | c <- s]
  putChar '\n'

main = do
  myPutStr "unchi"
