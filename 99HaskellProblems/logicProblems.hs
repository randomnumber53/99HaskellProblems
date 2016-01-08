-- Problem 46 --

not' :: Bool -> Bool
not' = not

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b 

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

impl' :: Bool -> Bool -> Bool
impl' False _ = True
impl' True True = True
impl' True False = False

eqal' :: Bool -> Bool -> Bool
eqal' a b = not' $ xor' a b

-- Problem 47 --


-- Problem 48 --


-- Problem 49 --


-- Problem 50 --