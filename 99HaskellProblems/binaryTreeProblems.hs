-- Defining the tree data type:

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

-- Problem 55 --

cbalTree :: (Integral a) => a -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
    | odd n =
        let halfTree = cbalTree $ quot n 2
        in [Branch 'x' x y | x <- halfTree, y <- halfTree]
    | otherwise =
        let smallHalf = cbalTree $ (quot n 2) - 1
            largeHalf = cbalTree $ (quot n 2)
        in [Branch 'x' x y | x <- smallHalf, y <- largeHalf] ++
           [Branch 'x' y x | x <- smallHalf, y <- largeHalf]

-- Problem 56 --

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ xl xr) (Branch _ yl yr) = 
    (mirror xr yl) && (mirror xl yr)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

-- Problem 57 --

constructTree :: (Ord a) => [a] -> Tree a
constructTree [] = Empty
constructTree xs =
    let top = head xs
        other = tail xs
        leftHalf = filter (< top) other
        rightHalf = filter (>= top) other
    in Branch top (constructTree leftHalf)
                  (constructTree rightHalf)

-- Problem 58 --

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty
reverseTree (Branch c l r) =
    Branch c (reverseTree r) (reverseTree l)

symCbalTrees :: (Integral a) => a -> [Tree Char]
symCbalTrees n
    | even n    = []
    | otherwise = [Branch 'x' t (reverseTree t) |
                   t <- (cbalTree $ quot n 2)]

-- Problem 59 --


-- Problem 60 --


-- Problem 61 --


-- Problem 62 --


-- Problem 63 --


-- Problem 64 --


-- Problem 65 --


-- Problem 66 --


-- Problem 67 --


-- Problem 68 --


-- Problem 69 --