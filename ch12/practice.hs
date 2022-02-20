data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
-- instance Applicative Tree where
--   pure = Leaf
--   (Leaf x) <*> x = Leaf x


-- instance Applicative Maybe where 
--   -- pure :: a -> Maybe a
--   pure = Just

--   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--   Nothing  <*> _  = Nothing
--   (Just g) <*> mx = fmap g mx


-- instance Applicative [] where 
--   -- pure :: a -> [a]
--   pure x = [x]

--   -- (<*>) :: [a -> b] -> [a] -> [b]
--   gs <*> xs = [g x | g <- gs, x <- xs]

prods xs ys zs = pure (*) <*> xs <*> (ys <*> zs)
prods2 xs ys zs = pure (*) <*> zs <*> prods xs ys



