myReadLine :: IO String
myReadLine = do
  c <- getChar 
  encoder "" c

encoder :: String -> Char -> IO String
encoder l c = case c of 
  '\DEL' -> do
    x <- getChar
    encoder (back l) x
  '\b' -> do 
    x <- getChar
    encoder (back l) x
  '\n' -> do 
    return l
  _ -> do 
    x <- getChar
    encoder (add l c) x

back :: String -> String
back l = take (length l - 1) l

add :: String -> Char -> String
add l c = l ++ [c]

main = do
  s <- myReadLine
  print s
