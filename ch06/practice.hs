fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

product :: Num a => [a] -> a
product []     = 1
product (n: ns) = n * Main.product ns

length :: [a] -> Int
length [] = 0
length(_: xs) = 1 + Main.length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x: xs) = Main.reverse xs Prelude.++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x: xs) ++ ys = x: (xs Main.++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y: ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x: xs) = insert x (isort xs)

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : Main.zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = Main.drop (n-1) xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller Main.++ [x] Main.++ qsort larger
  where 
    smaller = [a | a <- xs, a <= x] 
    larger =[b | b <- xs, b > x]

init :: [a] -> [a]
init [] = []
init [x] = []
init (x:xs) = x : Main.init xs

main = do
  print()
