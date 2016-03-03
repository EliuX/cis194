module HomeWork3.Golf (skips) where 

-- The first list in the output should be the same as the input list
-- The second list in the output should contain every second element from the input list
-- the n th list in the output should contain every nth element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips ls = [everyEach i ls | i <- [1..length ls]]

everyEach :: Int -> [a] -> [a]
everyEach 1 lst = lst 
everyEach n lst
        | n > 1 = [ lst!!(p-1) | p <-[n,(n+n)..(length lst)]] 
        | otherwise = [] 