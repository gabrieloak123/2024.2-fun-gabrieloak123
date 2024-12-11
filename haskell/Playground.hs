module Playground where

import Trace.Hpc.Mix (BoxLabel)
import Prelude hiding (
    foldl,
    foldr,
    zip,
 )

(°) :: (b -> c) -> (a -> b) -> a -> c
(f ° g) x = f $ g x

five :: Int -> Int
five _ = 5

double :: Int -> Int
double = (*) 2

quadruple :: Int -> Int
quadruple = double ° double

-- Hutton 5.2

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = divisors n == [1, n]

primesUntil :: Int -> [Int]
primesUntil n = [x | x <- [2 .. n], prime x]

findByKey :: (Eq a) => a -> [(a, b)] -> [b]
findByKey k t = [v | (k', v) <- t, k == k']

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : (zip xs ys)
zip _ _ = []

pairAdjacents :: [a] -> [(a, a)]
pairAdjacents xs = zip xs (tail xs)

sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairAdjacents xs]

findOccurrences :: (Eq a) => a -> [a] -> [Int]
findOccurrences x xs = [i | (x', i) <- zip xs [0 .. ((length xs) - 1)], x == x']

-- Hutton 7.2
listComprehensionMap :: (a -> b) -> [a] -> [b]
listComprehensionMap f xs = [f x | x <- xs]

listComprehensionFilter :: (a -> Bool) -> [a] -> [a]
listComprehensionFilter p xs = [x | x <- xs, p x]

foldr :: (a -> a -> a) -> a -> [a] -> a
foldr _ i [] = i
foldr o i (x : xs) = x `o` (foldr o i xs)

foldl :: (a -> a -> a) -> a -> [a] -> a
foldl _ i [] = i
foldl o i (x : xs) = (foldl o i xs) `o` x

snoc :: a -> [a] -> [a]
snoc n [] = [n]
snoc n (x : xs) = x : (snoc n xs)
