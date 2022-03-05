instance Functor ((->) a) where 
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap f g = f . g

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const
  -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x) 

-- Maybe a -> (a -> Maybe b) -> Maybe b
instance Monad ((->) a) where
  -- >>= :: (a -> b) -> (b -> a -> c) -> (a -> c)
  f >>= g = \x -> g (f x) x


