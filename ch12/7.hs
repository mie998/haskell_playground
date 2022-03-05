data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Var a) = Var (f a)
  fmap f (Val n) = Val n
  fmap f (Add a b) = Add (fmap f a) (fmap f b)

instance Applicative Expr where
  pure a = Var a

  (Var a) <*> mx = fmap a mx
  (Val n) <*> _ = Val n
  (Add l r) <*> mx = fmap (Add l r) mx

instance Monad Expr where
  (Var x) >>= g = g x
  (Val n) >>= g = Val n
  (Add l r) >>= g = Add (l >>= g) (r >>= g)
