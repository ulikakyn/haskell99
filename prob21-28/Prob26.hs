cnk :: Int -> [a] -> [[a]]
cnk _ [] = []
cnk 0 _  = []
cnk k xs = map lol (head xs) (help xs)

help :: [a] -> [[a]]
help [] = []
help (x:xs) = lol x (map (:[]) xs) ++ help xs

lol :: a -> [[a]] -> [[a]]
lol x xs = map (x:) xs
