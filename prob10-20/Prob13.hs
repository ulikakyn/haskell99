import qualified Data.Char as Char

encodeDirect :: Eq a => [a] -> [(String, a)]
encodeDirect [] = []
encodeDirect l@(x:xs) = (str,x) : encodeDirect (dropWhile (==x) xs)
  where str = if (counting l == 1) then "Single "
              else "Multiple " ++ [Char.intToDigit (counting l)]

counting :: Eq a => [a] -> Int
counting [] = 0
counting [x] = 1
counting (x:xs) = if (head xs == x) then 1 + counting xs
                  else 1
