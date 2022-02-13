data Tree a = Leaf a | Node (Tree a) (Tree a)

countLeaf :: Tree t -> Int
countLeaf (Leaf a) = 1
countLeaf (Node l r) = (countLeaf l) + (countLeaf r)

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node l r) = (countLeaf l) - (countLeaf r) <= 1

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take hl xs, drop hl xs) 
  where hl = length xs `div` 2

balance :: [a] -> Tree a
balance [] = error "cannnot enter empty list."
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where (l,r) = halve xs

tree2str :: Show t => Tree t -> String
tree2str (Leaf x) = "Leaf " ++ (show x)
tree2str (Node l r) = "Node (" ++ tree2str l ++ ") (" ++ tree2str r ++ ")"

main = do
  print (balanced (Leaf 3))
  print (balanced (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 5) (Leaf 1)))) (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 5) (Leaf 3))))))

  print(halve[1,2,3])
  print(halve[1,2,3,4])

  print(tree2str(balance[1,2,3]))
  print(tree2str(balance[1,2,3,4]))
  print(tree2str(balance[1,2,3,4,5]))
