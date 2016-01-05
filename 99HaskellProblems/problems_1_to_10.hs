-- Problem 1 --

myLast :: [a] -> a
myLast [] = error "Can't call myLast on an empty list."
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2 --

myButLast :: [a] -> a
myButLast [] = error "Can't call myButLast on an empty list."
myButLast [_] = error "Can't call myButLast on a one-element list."
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3 --

elementAt :: (Integral a) => [b] -> a -> b
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
-- Not sure how this works if n <= 0

-- Problem 4 --

myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5 --


-- Problem 6 --


-- Problem 7 --


-- Problem 8 --


-- Problem 9 --


-- Problem 10 --