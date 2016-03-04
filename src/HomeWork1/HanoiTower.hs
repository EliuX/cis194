{-# OPTIONS_GHC -Wall #-}
module HomeWork1.HanoiTower (hanoi) where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi size a b c
      |  size < 2 = [(a,b)]
      | otherwise = (hanoi notAPieces a c b) ++ (a,b):(hanoi notAPieces c b a)
      where notAPieces = size - 1
