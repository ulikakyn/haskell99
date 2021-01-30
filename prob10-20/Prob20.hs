removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing , [])
removeAt n xs
  | n < 1 = (Nothing , xs)
  | otherwise = (Just $ last $ take n xs, take (n-1) xs ++ drop n xs)
