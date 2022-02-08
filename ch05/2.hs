grid :: Int -> Int -> [(Int, Int)]
grid n m = [(n', m') | n' <- [0..n], m' <- [0..m]] 

main = do
  print(grid 1 2)
