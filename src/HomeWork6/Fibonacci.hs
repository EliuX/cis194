module HomeWork6.Fibonacci(fib, fibs1) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

fibs2 :: Stream Integer
fibs2 = Cons 0 (Cons 1 (fibseq 0 1))
 
data Stream a = Cons a (Stream a)
        deriving (Eq)

streamToList :: Stream a -> [a]
streamToList (Cons a nextStream) = a:(streamToList nextStream)
streamToList _                   = []

streamRepeat :: a -> Stream a
streamRepeat val = Cons val (streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a ns) = let a' = f a in Cons a' (streamMap f ns)

-- generates a Stream from a “seed” of type a
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = let a' = f a in Cons a (streamFromSeed f a')

instance (Show a) => Show (Stream a) where
 show stream = show (take 10000 (streamToList stream))

--which contains the infinite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

fibseq :: Integer -> Integer -> Stream Integer
fibseq p1 p2 = let c = (p1 + p2) in c `seq` Cons c (fibseq p2 c)

--interleaveStreams

-- where the nth element in the stream (assuming the first element
--   corresponds to n = 1) is the largest power of 2 which evenly
--   divides n. Ex.: 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
--ruler :: Stream Integer
--ruler = map (\(x:xs) -> Cons x ())

-- Converts an infit array into an stream
--arrayToStream :: [a] -> Stream a
--f
