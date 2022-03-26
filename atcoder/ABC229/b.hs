{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Char (isSpace)
import Data.List (sort)

main = do
  v <- getIntVector
  let a = (V.!) v 0
  let b = (V.!) v 1
  putStrLn $ solve a b

solve :: Integer -> Integer -> String
solve 0 b = "Easy"
solve a 0 = "Easy"
solve a b 
  | arem + brem >= 10 = "Hard"
  | otherwise = solve (a `div` 10) (b `div` 10)
  where 
    arem = a `mod` 10
    brem = b `mod` 10

getIntVector :: IO (V.Vector Integer)
getIntVector = V.unfoldr (B.readInteger . B.dropWhile isSpace) <$> B.getLine

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
