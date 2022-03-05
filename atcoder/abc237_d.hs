import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
import Data.List (group)
import Data.Foldable (toList)

main = do
  n <- getIntInLine
  s <- getStringInLine 
  putIntsInLine $ solve s 1 [] [0]

solve :: String -> Int -> [Int] -> [Int] -> [Int]
solve "" _ l r = l ++ r
solve (x:xs) n l r = case x of
  'L' -> solve xs (n+1) l (n:r)
  'R' -> solve xs (n+1) (l ++ [head r]) (head r : n : drop 1 r)
  _ -> []

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
