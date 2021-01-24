module Prob11 (encodeModified) where
  import Prob10


  encodeModified :: Eq a => [a] -> [(String,a)]
  encodeModified xs = map singleOrMultiple (encode xs)

  singleOrMultiple :: (Int,a) -> (String,a)
  singleOrMultiple (n,a) = if n==1 then ("Single ",a)
                       else ("Multiple " ++ show n, a)
