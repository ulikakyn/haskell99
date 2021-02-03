import System.Random

main = do
  gen <- getStdGen
  putStrLn "input a list"
  input <- getLine
  putStrLn "how many do you want to select from above list?"
  count <- getLine
  --rnd_select' gen input
  putStrLn $ rnd_select gen input count

rnd_select :: StdGen -> String -> String -> String
rnd_select gen xs n = foldr (\m acc -> (xs !! m) : acc) [] rand_list
  where rand_list = take (read n) (randomRs (0, length xs-1) gen)

rnd_select' :: StdGen -> String -> IO ()
rnd_select' gen xs = putStrLn $ [(xs !! fst randn)]
  where randn = randomR (0, length xs - 1) (gen)
