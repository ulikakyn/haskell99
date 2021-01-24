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
pack l@(x:xs) = (x : mytakeWhile (== x) xs) : pack (mydropWhile (== x) xs)
