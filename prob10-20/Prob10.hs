module Prob10 (encode) where

  compress :: (Eq a) => [a] -> [a]
  compress [] = []
  compress xs = [ fst (a,b) | (a,b) <- ys, a /= b] ++ [last xs]
    where ys = zip xs (tail xs)

  mydropWhile :: (a -> Bool) -> [a] -> [a]
  mydropWhile p [] = []
  mydropWhile p l@(x:xs)
    | p $ x = mydropWhile p xs
    | otherwise = l

  mytakeWhile :: (a -> Bool) -> [a] -> [a]
  mytakeWhile p [] = []
  mytakeWhile p (x:xs)
    | p x = x : mytakeWhile p xs
    | otherwise = []

  pack :: (Eq a) => [a] -> [[a]]
  pack [] = []
  pack (x:xs) = (x : mytakeWhile (== x) xs) : pack (mydropWhile (== x) xs)

  encode :: Eq a => [a] -> [(Int,a)]
  encode [] = []
  encode xs = zip lengt cxs
    where lengt = map length pxs
          pxs = pack xs
          cxs = compress xs
