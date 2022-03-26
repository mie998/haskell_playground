
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Set as S

main = do
  n <- getIntInLine
  let set = S.fromList [""]
  vo <- V.replicateM n B.getLine
  print (solve vo set - 1)

solve :: V.Vector B.ByteString -> S.Set B.ByteString -> Int
solve v set
  | V.null v = S.size set
  | otherwise = solve (V.tail v) newSet
      where
        newSet = S.insert (V.head v) set

readIntFromBS :: B.ByteString -> Int
readIntFromBS = fst . fromJust . B.readInt

getIntInLine :: IO Int
getIntInLine = readIntFromBS <$> B.getLine

getIntVector :: IO (VU.Vector Int)
getIntVector = VU.unfoldr (B.readInt . B.dropWhile isSpace) <$> B.getLine

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
