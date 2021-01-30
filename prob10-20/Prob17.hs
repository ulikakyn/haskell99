import Prob16

splitAt' :: [a] -> Int -> ([a],[a])
splitAt' xs n
  | n < 1 = ([] , xs)
  | n > length xs = (xs , [])
  | otherwise = (take' n xs, drop' n xs)
