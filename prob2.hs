myButLast :: [a] -> a
myButLast [] = error "empty"
myButLast [x] = error "there is no but last"
myButLast [x,y] = x
myButLast (x:y:xs) = myButLast ys
  where ys = y:xs

--improvement

myButLast' :: [a] -> a
myButLast' [] = error "empty"
myButLast' [x] = error "there is no but last"
myButLast' [x,_] = x
myButLast' (_:xs) = myButLast' xs
