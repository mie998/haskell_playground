import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
import Data.List (group)
import Data.Foldable (toList)

main = do
  s <- getStringInLine 
  let n = read s :: Integer
  let m = 998244353 :: Integer
  let len = length s 
  let dl = 10^(len-1) 
  let ans = getSum (len - 1) m + cusumMod dl n m - ((n - dl + 1) * (dl - 1)) `mod` m
  print $ ans `mod` m 

getSum :: Int -> Integer -> Integer
getSum 0 _ = 0
getSum 1 _ = 45
getSum digit m = 
  (sum + getSum (digit - 1) m) `mod` m
  where 
    sum = cusumMod dl (du - 1) m - ((du - dl) * (dl - 1))
    du = 10^digit 
    dl = 10^(digit-1) 


cusumMod :: Integer -> Integer -> Integer -> Integer 
cusumMod a b m = 
  ((b - a + 1) * (a + b) `div` 2) `mod` m


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
