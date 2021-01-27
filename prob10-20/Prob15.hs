repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n
  | n < 1 = []
  | otherwise = duplin x n ++ repli xs n

duplin :: a -> Int -> [a]
dupli x n
  | n < 1 = []
  | otherwise = x : dupli x (n-1)
