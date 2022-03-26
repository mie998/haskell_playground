{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromJust)
import System.IO (stdout)
import qualified Data.Vector.Unboxed as VU
import Data.Char ()
import Data.List (sort)


main = do
  let s = "unchi"
  let bs = B.pack s
  let ts = T.pack s
  putStrLn $ B.unpack bs
  print ts
