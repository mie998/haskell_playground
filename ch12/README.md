Applicative であることは Functor 則を満たすかつ Applicative rules を満たす `pure`, `<*>` の組が定義されていることを言う。

Applicative rules:
```haskell
pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y)  <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
```
