-- Problem 31 --

isPrime :: (Integral a) => a -> Bool
isPrime n =
    let checkUpTo = [2..(ceiling . sqrt . fromIntegral $ n)]
    in and [n `mod` x /= 0 | x <- checkUpTo]

-- Problem 32 --


-- Problem 33 --


-- Problem 34 --


-- Problem 35 --


-- Problem 36 --


-- Problem 37 --


-- Problem 38 --


-- Problem 39 --


-- Problem 40 --