null :: [a] -> Bool
null [] = True
null (x:xs) = False

safetail1 xs = if Main.null xs then [] else tail xs
safetail2 xs | Main.null xs = []
             | otherwise = tail xs
safetail3 [] = []
safetail3 (_:xs) = xs

main = do
  print $ safetail1 [1,2,3,4]
  print $ safetail1 ([]::[Int])
  print $ safetail2 [1,2,3,4]
  print $ safetail2 ([]::[Int])
  print $ safetail3 [1,2,3,4]
  print $ safetail3 ([]::[Int])
