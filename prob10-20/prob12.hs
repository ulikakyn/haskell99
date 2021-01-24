import qualified Data.Char as Char
import Prob11

decodeModified :: Eq a => [(String,a)] -> [a]
decodeModified [] = []
decodeModified (x:xs) = replicate n y ++ decodeModified xs
  where y = snd x
        n = if fst x == "Single " then 1 else l
          where l = Char.digitToInt $ head $ dropWhile (not . Char.isDigit) (fst x)
