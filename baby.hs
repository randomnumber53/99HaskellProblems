doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char] 
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list."
head' (x:_) = x

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
    | bmi <= small = "S"
    | bmi <= medium = "M"
    | bmi <= large = "L"
    | otherwise         = "XL"
    where bmi = w / h ^ 2 
          small = 18.5
          medium = 25.0
          large = 30.0

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerThan = quicksort [e | e <- xs, e <= x]
        greaterThan = quicksort [e | e <- xs, e > x]
    in smallerThan ++ [x] ++ greaterThan

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (3 * n + 1)

-- Project Euler #27 --

isPrime :: (Integral a) => a -> Bool
isPrime n =
    let checkUpTo = [2..(ceiling . sqrt . fromIntegral  $ n)]
    in and [n `mod` x /= 0 | x <- checkUpTo]

lenPrimeList :: (Integral a) => a -> a -> Int
lenPrimeList a b = 
    length (takeWhile isPrime (map (\x -> x^2 + a*x + b) [0..]))

maxPrimes = maximum [lenPrimeList a b | a <- [-999..999], b <- [-999..999]]

-- From Gabe: 

binSearch :: (Ord a) => a -> [a] -> Bool
binSearch s [] = False
binSearch s [x] = s == x
binSearch s y@(x:xs)
            | y !! half > s = binSearch s (take half y)
            | y !! half < s = binSearch s (drop half y)
            | otherwise = True
            where half = length y `quot` 2
