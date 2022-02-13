import Data.Char

type Bit = Int
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

countParity :: [Bit] -> Int
countParity [] = 0
countParity (x:xs) = (if x == 1 then 1 else 0 + countParity xs) `mod` 2

appendParity :: [Bit] -> [Bit] 
appendParity bits = bits ++ [countParity bits] 

evalParity :: [Bit] -> [Bit]
evalParity bits = case countParity bits of
  0 -> bits
  1 -> error ("Parity is invalid... some data corruption may happened")

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

bitHole :: [Bit] -> [Bit]
bitHole bits = take (length bits - 1) bits

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . evalParity .bitHole . appendParity . encode

main = do
  print (transmit "unchi")
