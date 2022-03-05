import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])
