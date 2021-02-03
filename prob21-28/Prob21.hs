insertAt :: a -> [a] -> Int -> [a]
insertAt y xs@(z:zs) n
  | n < 1 = y:xs
  | n > length xs = xs ++ [y]
  | n == 1 = y:z:zs
  | otherwise = z : insertAt y zs (n-1)
