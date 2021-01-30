module Prob16
(dropEvery
, take'
, drop'
) where

  dropEvery :: [a] -> Int -> [a]
  dropEvery [] _ = []
  dropEvery xs n
    | n < 1 = xs
    | n > length xs = xs
    | otherwise = take' (n-1) xs ++ dropEvery (drop' n xs) n

  take' :: Int -> [a] -> [a]
  take' _ [] = []
  take' n (x:xs)
    | n < 1 = []
    | otherwise = x : take' (n-1) xs

  drop' :: Int -> [a] -> [a]
  drop' _ [] = []
  drop' n l@(_:xs)
    | n < 1 = l
    | otherwise = drop' (n-1) xs
