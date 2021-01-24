compress :: (Eq a) => [a] -> [a]
compress [] = []
--compress [x] = [x]
--compress (x:y:xs)
--  | x /= y = x : []
--  | otherwise = compress xs

compress xs = [ fst (a,b) | (a,b) <- ys, a /= b] ++ [last xs]
  where ys = zip xs (tail xs)
