{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.Char (isSpace)
import Data.List (unfoldr)

main = do
  [n,w] <- getIntList
  vo <- VU.replicateM n ((\x -> (x !! 0, x !! 1)) <$> getIntList)
  let v = VU.modify (VAI.sortBy (flip compare)) vo
  print $ step w v

step :: (VU.Unbox b, Ord b, Num b) => b -> VU.Vector (b, b) -> b
step 0 _ = 0
step w vs 
  | VU.null vs = 0
  | otherwise = score + step (w - amount) (VU.tail vs)
  where
    t = VU.head vs
    amount = min w (snd t)
    score = amount * fst t
  

readIntFromBS :: B.ByteString -> Int
readIntFromBS = fst . fromJust . B.readInt

getIntInLine :: IO Int
getIntInLine = readIntFromBS <$> B.getLine

getIntVector :: IO (VU.Vector Int)
getIntVector = VU.unfoldr (B.readInt . B.dropWhile isSpace) <$> B.getLine

getIntList :: IO [Int]
getIntList = unfoldr (B.readInt . B.dropWhile isSpace) <$> B.getLine

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
