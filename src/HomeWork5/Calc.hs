module HomeWork5.Calc(eval, evalStr, reify, lit, mul, add) where

import HomeWork5.ExprT
import HomeWork5.Parser


class Calculable a where
 eval :: a -> Integer

class Expr a where
 lit :: Integer -> a
 mul :: a -> a -> a
 add :: a -> a -> a

instance Expr ExprT where
 lit x = Lit x
 mul x y = Mul x y
 add x y = Add x y

instance Calculable ExprT where
 -- evals an ExprT expression calculating the contained math expression in it	 
 eval (Lit val) = val
 eval (Mul a b) = (eval a) * (eval b)         
 eval (Add a b) = (eval a) + (eval b)

-- Produces Nothing for inputs which are not well-formed expressions, and
-- Just n for well-formed inputs that evaluate to n.
evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of
                Just a -> Just (eval a)
                _      -> Nothing

reify :: ExprT -> ExprT
reify = id