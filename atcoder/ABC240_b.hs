import Data.List
import qualified Data.Set as S

main = do
  getLine
  as <- map read . words <$> getLine :: IO [Int]
  print(S.size . S.fromList $ as)


