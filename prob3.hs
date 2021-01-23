elementAt :: [a] -> Int -> a
elementAt (x:xs) n = case n of 1 -> x
                               n -> elementAt xs (n-1)
