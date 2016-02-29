module HomeWork1.CreditCardValidation (toDigits, doubleEveryOther)  where

toDigits :: Integer -> [Integer]
toDigits num
      | num >= 10 = (toDigits (div num 10)) ++ [mod num 10]
      | num <= 0 = []
      | otherwise = [num]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nl = [ if (isEvenBackwards position total) then val*2 else val  | (position, val)<-nlTuples]
                      where total = length nl
                            nlTuples = zip [1..total] nl

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

isEven :: Int -> Bool
isEven x = mod x 2 == 0

isEvenBackwards :: Int -> Int -> Bool
isEvenBackwards position total = isEven (total - position + 1)
