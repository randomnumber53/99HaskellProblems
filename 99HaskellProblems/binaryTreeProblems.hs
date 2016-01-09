-- Defining the tree data type:

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

leaf = (Branch 'x' Empty Empty)
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

hbalTree :: (Integral b) => a -> b -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree e 1 = [Branch e Empty Empty]
hbalTree e h =
    [Branch e l r |
     (hl, hr) <- [(h-1, h-1),(h-1, h-2),(h-2, h-1)],
     l <- hbalTree e hl, r <- hbalTree e hr]

-- Problem 60 --

-- Skipped...for now.

-- Problem 61 --

countLeaves :: (Integral b) => Tree a -> b
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch e Empty Empty) = [e]
leaves (Branch _ l r) = leaves l ++ leaves r

-- Problem 62 --

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch e l r) = e : (internals l ++ internals r)

atLevel :: (Integral b) => Tree a -> b -> [a]
atLevel Empty _ = []
atLevel (Branch e _ _) 1 = [e]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

-- Problem 63 --

completeBinaryTree :: (Integral a) => a -> Tree Char
completeBinaryTree n = genTree 1
    where genTree x
            | x > n     = Empty
            | otherwise = Branch 'x' (genTree (2*x))
                                     (genTree (2*x + 1))

countNodes  :: (Integral b) => Tree a -> b
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

-- I don't think this is the most efficent or clear.
-- It does appear to work, though.

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree (Branch _ l r) =
    let sizeL = countNodes l
        sizeR = countNodes r
        pows = take (sizeL+1) [2^i-1 | i <- [0..]]
    in and [isCompleteBinaryTree l, isCompleteBinaryTree r,
            sizeL >= sizeR,  2*sizeR + 1 >= sizeL,
            if (not $ elem sizeR pows)
                then (elem sizeL pows)
                else True]

-- Problem 64 --


-- Problem 65 --


-- Problem 66 --


-- Problem 67 --


-- Problem 68 --


-- Problem 69 --