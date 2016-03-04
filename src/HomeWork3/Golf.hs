module HomeWork3.Golf (skips, localMaxima, histogram) where

-- The first list in the output should be the same as the input list
-- The second list in the output should contain every second element from the input list
-- the n th list in the output should contain every nth element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips ls = [everyEach i ls | i <- [1..length ls]]

-- Devuelve todos los elementos a cada posicion n de una lista lst
everyEach :: Int -> [a] -> [a]
everyEach n lst = case drop (n-1) lst of
                    []     -> []
                    (x:xs) -> x:(everyEach n xs)

--A list of list of differents combinations of the passed list--
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest)
           | a < b && b > c = b:(localMaxima nextGroup)
           | otherwise      = localMaxima nextGroup
           where nextGroup  = b:c:rest
localMaxima _               = []

-- takes as input a list of Integers between 0 and 9 (inclusive),and outputs a vertical histogram 
-- showing how many of each number were in the input list
histogram :: [Int] -> String
histogram numList = printHist (histogramMap numList) 

histogramMap :: [Int] -> [Int]
histogramMap numList = [length(filter (==n) numList) | n <- [1..9]]

printHistLine :: Int -> [Int] -> [Char]
printHistLine ln timesList = [if ln <= times then '*' else ' ' | times <- timesList] 

printHist :: [Int] -> [Char] 
printHist timesList@(x:_) = let maxVal = maximum timesList in unlines([printHistLine n timesList | n <- [maxVal, maxVal-1..1]] ++ ["==========","0123456789"]) 
printHist _ = ""            