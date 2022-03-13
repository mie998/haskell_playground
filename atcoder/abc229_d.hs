
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
import Data.List (group)
import Data.Array
import Data.Foldable (toList)

main = do
  s <- getStringInLine 
  k <- getIntInLine
  let n = length s
  let xs = listArray (0,n) $ scanl countDot 0 s 
  print $ resolve n k xs 1 1

countDot :: Int -> Char -> Int
countDot n c = if c == '.' then n+1 else n

resolve n k xs l r 
  | score >= n || r > n = score
  | rx - lx <= k || l > r = max score (resolve n k xs l (r+1))
  | rx - lx > k = max score (resolve n k xs (l+1) r) 
  | otherwise = score
    where 
      lx = xs ! (l - 1)
      rx = xs ! r
      score = r - l
 
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
