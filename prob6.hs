myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' xs = myhelp xs []
  where myhelp [] ys = ys
        myhelp (x:xs) ys = myhelp xs (x:ys)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome ys
  | ys == myReverse' ys = True
  | otherwise = False
