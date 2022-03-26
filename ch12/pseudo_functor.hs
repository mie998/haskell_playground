data List a = Nil | Cons a (List a)

-- これがエラーになる意味がわからん。
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons x (fmap f xs)

main = do
  let l = LL 3;
  print l


