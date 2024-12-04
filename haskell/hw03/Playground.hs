module Playground where

import Prelude

(°) :: (b -> c) -> (a -> b) -> a -> c
(f ° g) x = f $ g x

five :: Int -> Int
five _ = 5

double :: Int -> Int
double = (*) 2

quadruple :: Int -> Int
quadruple = double ° double
