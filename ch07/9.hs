altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) = [f x] ++ [g y] ++ altMap f g xs

main = do
  print(altMap (+10) (+100) [0,1,2,3])
  print(altMap (+10) (+100) [0,1,2,3,4])
  print(altMap (+10) (+100) [0,1,2,3,4,5])
