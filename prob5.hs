myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' xs = myhelp xs []
  where myhelp [] ys = ys
        myhelp (x:xs) ys = myhelp xs (x:ys)
