module HomeWork6.Fibonacci(fib, fibs1) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

fibs2 :: [Integer]
fibs2 = [fib x | x <- [0..]]

data Stream a = Cons a (Stream a)
        deriving (Show, Eq)

--streamToList :: Stream a -> [a]
--streamToList a =

streamRepeat :: a -> Stream a

instance Show (Stream a) where
 show