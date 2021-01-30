slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs n m
  | (n > length xs) || (m < 1) = []
  | n < 1 = if (m < length xs) then take m xs else xs
  | otherwise = take (m-n+1) $ drop (n-1) xs
