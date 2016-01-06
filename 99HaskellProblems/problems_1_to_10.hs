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
elementAt [] _ = error "Index out of range."
elementAt (x:_) 1 = x
elementAt (_:xs) n
    | n <  1 = error "Index must be >= 1."
    | n >= 1 = elementAt xs (n - 1)

-- Problem 4 --

myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5 --

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6 --

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (xs == reverse xs)

-- Problem 7 --

data NestedList a = Elem a | List [NestedList a]

-- Problem 8 --


-- Problem 9 --


-- Problem 10 --