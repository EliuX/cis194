module HomeWork5.Calc(eval, evalStr) where

import HomeWork5.ExprT
import HomeWork5.Parser


class Calculable a where
 eval :: a -> Integer

class Expr a where
 lit :: a
 mul :: a
 add :: a

instance Expr (Calculable ExprT) where
 lit = Lit
 mul = Mul
 add = Add

-- evals an ExprT expression calculating the contained math expression in it
instance Calculable ExprT where
 eval (Lit val) = val
 eval (Mul a b) = (eval a) * (eval b)
 eval (Add a b) = (eval a) + (eval b)

-- Produces Nothing for inputs which are not well-formed expressions, and
-- Just n for well-formed inputs that evaluate to n.
evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of
                Just a -> Just (eval a)
                _      -> Nothing
