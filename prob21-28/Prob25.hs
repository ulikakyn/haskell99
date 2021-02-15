import System.Random

main = do
  gen <- getStdGen
  putStrLn "What list should i permute?"
  list <- getLine
  print $ create_rand_list gen list


create_rand_list :: StdGen -> [a] -> [a]
create_rand_list _ [] = []
create_rand_list gen xs = (xs !! n) : create_rand_list m leftoverlist
  where (n,m) = randomR (0, length xs-1) gen
        leftoverlist = take n xs ++ drop (n+1) xs
