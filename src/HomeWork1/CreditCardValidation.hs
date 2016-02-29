module HomeWork1.CreditCardValidation (toDigits, doubleEveryOther, sumDigits, validate)  where

import Common

--converts an Integer to a List of Digits--
toDigits :: Integer -> [Integer]
toDigits num
      | num >= 10 = (toDigits (div num 10)) ++ [mod num 10]
      | num <= 0 = []
      | otherwise = [num]

--should double every other number beginning from the right--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nl = [ if (isEvenBackwards position total) then val*2 else val  | (position, val)<-nlTuples]
                      where total = length nl
                            nlTuples = zip [1..total] nl

--to calculate the sum of all digits--
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:rest) = sum (toDigits x) + sumDigits rest

--Validates if a credit card is valid--
validate :: Integer -> Bool
validate cardNum = (sumDigits ( doubleEveryOther (toDigits cardNum))) `mod` 10 == 0
