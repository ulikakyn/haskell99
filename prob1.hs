tail' :: [a] -> a
tail' [] = error "there is no tail"
tail' [x] = x
tail' (x:xs) = tail' xs
