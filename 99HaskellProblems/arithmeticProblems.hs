-- Problem 31 --

isPrime :: (Integral a) => a -> Bool
isPrime n =
    let checkUpTo = [2..(ceiling . sqrt . fromIntegral $ n)]
    in and [n `mod` x /= 0 | x <- checkUpTo]

-- Problem 32 --

myGCD :: (Integral a) => a -> a -> a
myGCD x y
    | x == 0    = y
    | y == 0    = x
    | otherwise = let smaller = abs $ min x y
                      r = (abs $ max x y) `mod` smaller
                  in myGCD smaller r

-- Problem 33 --

coprime :: (Integral a) => a -> a -> Bool
coprime x y = 1 == myGCD x y

-- Problem 34 --

totient :: (Integral a) => a -> Int
totient 1 = 1
totient n = length [e | e <- [1..n], coprime e n]

-- Problem 35 --

primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n =
    let divisor = head [x | x <- [2..], (n `mod` x) == 0]
    in divisor : (primeFactors $ quot n divisor)

-- Problem 36 --

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack all@(x:xs) = (takeWhile (== x) all) : pack (dropWhile (== x) xs)

encode :: (Eq a) => [a] -> [(a, Int)]
encode xs = map (\xs -> (head xs, length xs)) $ pack xs

primeFactorsMult :: (Integral a) => a -> [(a, Int)]
primeFactorsMult = encode . primeFactors

-- Problem 37 --

fastTotient :: (Integral a) => a -> a
fastTotient 1 = 1
fastTotient n =
    let pFactors = primeFactorsMult n
    in foldl (\acc x -> let p = (fst x)
                            k = (snd x)
                        in acc*(p^k-p^(k-1))) 1 pFactors

-- Problem 38 --

-- No solution needed.

-- Problem 39 --

primesR :: (Integral a) => a -> a -> [a]
primesR x y = filter isPrime [x..y]

-- Problem 40 --

goldbach :: (Integral a) => a -> (a,a)
goldbach n 
    | n < 3     = error "Input must be at least 3."
    | otherwise =
        let choices = primesR 2 n
        in head [(x, y) | x <- choices, y <- choices,
                 (x + y) == n]

-- Problem 41 --

goldbachList :: (Integral a) => a -> a -> [(a,a)]
goldbachList x y = map goldbach range
                   where range = filter even [x..y]