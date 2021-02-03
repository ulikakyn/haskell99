import System.Random

main = do
  gen <- getStdGen
  putStrLn "input a list"
  input <- getLine
  rnd_select' gen input

--rnd_select :: [a] -> Int -> [a]
--let rand_list = take n (randomRs (0, length xs) (mkStdGen 100))
--rnd_select xs n = map

rnd_select' :: StdGen -> String -> IO ()
rnd_select' gen xs = putStrLn $ [(xs !! fst randn)]
  where randn = randomR (0, length xs - 1) (gen)
