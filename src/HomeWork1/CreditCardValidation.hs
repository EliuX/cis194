module HomeWork1.CreditCardValidation (toDigits)  where 

toDigits :: Integer -> [Integer]
toDigits x = [(read [l] :: Integer)| l <- (show x)]
