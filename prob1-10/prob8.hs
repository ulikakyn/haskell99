compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = [ fst (a,b) | (a,b) <- ys, a /= b] ++ [last xs]
  where ys = zip xs (tail xs)
