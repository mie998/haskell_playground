adder :: IO()
adder = do
  putStr "How many numbers?: "
  l <- getLine
  let n = read l :: Int
  accumlator n 0

accumlator :: Int -> Int -> IO()
accumlator 0 s = print ("The total is: "  ++ show s)
accumlator n s = do
  l <- getLine 
  let x = read l :: Int
  accumlator (n-1) (s + x)

main = do
  adder

