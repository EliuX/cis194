module HomeWork5.Calc(eval, evalStr, lit, mul, add, printInt, printBool) where

import HomeWork5.ExprT
import HomeWork5.MinMax
import HomeWork5.Mod7
import HomeWork5.Parser

class Calculable a where
 eval :: a -> Integer

class (Show a) => Expr a where
 lit :: Integer -> a
 mul :: a -> a -> a
 add :: a -> a -> a

instance Expr ExprT where
 lit x = Lit x
 mul x y = Mul x y
 add x y = Add x y

-- evals an ExprT expression calculating the contained math expression in it
instance Calculable ExprT where
 eval (Lit val) = val
 eval (Mul a b) = (eval a) * (eval b)
 eval (Add a b) = (eval a) + (eval b)

-- Instance of Integer
instance Expr Integer where
 lit = id
 mul x y = x * y
 add x y = x + y

-- Instance of Bool
instance Expr Bool where
 lit x
    | (x>0)= True
    | otherwise = False
 mul x y = x || y
 add x y = x && y

-- Instance for MinMax
instance Expr MinMax where
 lit x = MinMax x
 mul (MinMax x) (MinMax y) = MinMax (min x y)
 add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
 lit x = Mod7 (mod x 7)
 mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)
 add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)

-- Produces Nothing for inputs which are not well-formed expressions, and
-- Just n for well-formed inputs that evaluate to n.
evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of
                Just a -> Just (eval a)
                _      -> Nothing

printInt :: Integer -> Integer
printInt = id
printBool :: Bool -> Bool
printBool = id
