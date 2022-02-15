act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine :: IO String
getLine = do 
  x <- getChar
  if x == '\n' 
    then return []
  else 
    do 
      xs <- Main.getLine
      return (x:xs)

strlen :: IO ()
strlen = do 
  putStr "Enter a string: "
  xs <- Prelude.getLine
  putStr "The string has " 
  putStr (show (length xs)) 
  putStrLn " characters"

main = do 
  x <- act
  y <- Main.getLine
  print y

