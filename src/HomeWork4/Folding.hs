module HomeWork4.Folding(fun1, fun2, fun1', fun2') where

-- Multiplies every even number decreased in two
fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map ((-2)+) . filter even 

-- Given a number 'n' if its even it plus it and apply the function to its half, else case it aplies the function to its triples plus one
fun2 :: Integer -> Integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer 
fun2' = sum . takeWhile (>0) . map (`div` 2) . filter even . iterate ((+1) . (*3))