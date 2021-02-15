import System.Random

main = do
  gen <- getStdGen
  putStrLn "How big should the range be?"
  range <- getLine
  putStrLn "How many should i draw?"
  count <- getLine
  print $ rnd_select' gen (read count) (read range)

rnd_select' :: StdGen -> Int -> Int -> [Int]
rnd_select' gen n m
  | (n < 1) || (m < 1) = []
  | otherwise = take n (randomRs (1, m) gen)
