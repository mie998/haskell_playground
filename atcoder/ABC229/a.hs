{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU

main = do
  s1 <- B.getLine
  s2 <- B.getLine
  let ans = if solve s1 s2 then "Yes" else "No"
  putStrLn ans

solve s1 s2
  | s1 == fst antiPattern && s2 == snd antiPattern = False
  | s1 == snd antiPattern && s2 == fst antiPattern = False
  | otherwise = True
  where
    antiPattern = (".#", "#.")

readIntFromBS :: B.ByteString -> Int
readIntFromBS = fst . fromJust . B.readInt

getIntInLine :: IO Int
getIntInLine = readIntFromBS <$> B.getLine

getStringInLine :: IO String
getStringInLine = B.unpack <$> B.getLine

renderInts :: [Int] -> B.Builder
renderInts []     = mempty
renderInts (x:xs) = B.intDec x <> mconcat [ B.charUtf8 ' ' <> B.intDec x' | x' <- xs ]

addNewLine :: B.Builder -> B.Builder
addNewLine = (<> B.charUtf8 '\n')

putBuilder :: B.Builder -> IO ()
putBuilder = B.hPutBuilder stdout

putIntsInLine :: [Int] -> IO ()
putIntsInLine = putBuilder . addNewLine . renderInts
