module HomeWork4.Folding(fun1, fun2, fun1', fun2', foldTree, isBalanced, myFoldl, xor, map', sieveSundaram) where

-- Multiplies every even number decreased in two
fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map ((-2)+) . filter even

-- Given a number 'n' if its even it plus it and apply the function to its half, else case it aplies the function to
-- its triples plus one
fun2 :: Integer -> Integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2 )
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else  3 * x + 1)

-- Tree definition
data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

-- Generates a balanced tree
foldTree :: [a] -> Tree a
foldTree x = foldr putInTree Leaf x


putInTree :: a -> Tree a -> Tree a
putInTree a  Leaf                 = Node 0 Leaf a Leaf
putInTree a (Node h Leaf x rn)    = Node (max h 1) (Node 0 Leaf a Leaf) x rn
putInTree a (Node h ln x Leaf)    = Node (max h 1) ln x (Node 0 Leaf a Leaf)
putInTree a (Node h ln x rn)
            | gotoLeft            = Node newHeight changedNode x rn
            | otherwise           = Node newHeight ln x changedNode
            where gotoLeft        = heightOf ln < heightOf rn
                  leatDeepNode    = if heightOf ln < heightOf rn then ln else rn
                  changedNode@(Node sh _ _ _) = putInTree a leatDeepNode
                  newHeight                   = max h sh + 1

heightOf :: Tree a -> Integer
heightOf Leaf = 0
heightOf (Node height _ _ _) = height

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ lnode _ rnode) = abs ((heightOf lnode) - (heightOf rnode)) <= 1

-- My implementation of foldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Count of True values must be odd
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- Must behave as map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f(a):b) []

-- Odd numbers until

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- Give a list of prime numbers until an integer 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\(_, oddNum) -> oddNum ) . filter (\(oddmaskList, num) -> num `notElem` oddmaskList) . primesUntil

primesUntil :: Integer -> [([Integer],Integer)]
primesUntil n =  cartProd [[(2*i + 1)*(2*j + 1) | j <- [1..n], i <- [1..j]]]  [ 2*k + 1 | k <- [1..n]]