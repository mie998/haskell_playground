
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

main = do
  print (nat2int(mult (int2nat 3) (int2nat 4)))
  print (nat2int(mult (int2nat 0) (int2nat 4)))
  print (nat2int(mult (int2nat 3) (int2nat 0)))

