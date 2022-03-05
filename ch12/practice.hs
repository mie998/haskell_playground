-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--               deriving Show
-- instance Functor Tree where
--   fmap f (Leaf x) = Leaf (f x)
--   fmap f (Node l r) = Node (fmap f l) (fmap f r)


-- data Maybe2 = Just | Nothing
-- instance Applicative Maybe2 where 
--   -- pure :: a -> Maybe a
--   pure = Main.Just

--   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--   Main.Nothing  <*> _  = Main.Nothing
--   (Main.Just g) <*> mx = fmap g mx

-- instance Applicative [] where 
--   -- pure :: a -> [a]
--   pure x = [x]

--   -- (<*>) :: [a -> b] -> [a] -> [b]
--   gs <*> xs = [g x | g <- gs, x <- xs]

prods xs ys zs = pure (*) <*> xs <*> (ys <*> zs)
prods2 xs ys zs = pure (*) <*> zs <*> prods xs ys


data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m

type State = Int
-- type ST a = State -> (a, State)
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) = st

instance Functor ST where
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S (\s -> (x,s))
  stf <*> stx = S (\s ->
    let (f,s') = app stf s
        (x,s'') = app stx s' in (f x, s'')) 

instance Monad ST where
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where 
    (l',n') = rlabel l n
    (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do Leaf <$> fresh
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- Main.mapM f xs
  return (y:ys)

