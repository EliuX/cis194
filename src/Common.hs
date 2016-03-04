{-# OPTIONS_GHC -Wall #-}
module Common (isEven, isEvenBackwards)  where

-- Is a number Even? --
isEven :: Int -> Bool
isEven x = mod x 2 == 0

-- Is the position of an total in an even position from right to left? --
isEvenBackwards :: Int -> Int -> Bool
isEvenBackwards position total = isEven (total - position + 1)
