module HomeWork6.Fibonacci(fib, fibsl) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibsl :: [Integer]
fibsl = [fib x | x <- [0..]]