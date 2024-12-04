module Ints where

import Prelude hiding (
  Int,
 )

import Nat

data Int where
  Int :: Nat -> Nat -> Int

instance Show Int where
  show (Int n O) = show n
  show (Int O n) = "-" ++ show n
  show i = show (Cannon i)

instance Eq Int where
  

Cannon :: Int -> Int
Cannon (Int (S n) (S m)) = Cannon (Int n m)
Cannon n = n

para todo n, para toda lista, fi de n && fi da lista -> fi cons x lista
