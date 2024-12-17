module Origami where

import Prelude hiding (
    all,
    and,
    any,
    concat,
    dropWhile,
    filter,
    foldl,
    foldl1,
    foldr,
    foldr1,
    length,
    map,
    or,
    product,
    scanl,
    scanr,
    sum,
    takeWhile,
 )

import Prelude qualified as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> a -> a) -> a -> [a] -> a
foldr o i l = case l of
    [] -> i
    (x : xs) -> x `o` (foldr o i xs)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (a -> a -> a) -> a -> [a] -> a
foldl o i l = case l of
    [] -> i
    (x : xs) -> (foldl o i xs) `o` x

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 o l = case l of
    [x] -> x
    (x : xs) -> x `o` (foldr1 o xs)

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 o l = case l of
    [x] -> x
    (x : xs) -> (foldl1 o xs) `o` x

--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl o a l = case l of
    [] -> []
    (x : xs) -> a' : (scanl o a' xs) where a' = a `o` x

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr o i l = case l of {}

--
-- Define all of the following functions as folds:
--

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

minimum :: (Ord a) => [a] -> a
minimum = foldr1 min

maximum :: (Ord a) => [a] -> a
maximum = foldr1 max

length :: (Integral i) => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter p = undefined

map :: (a -> b) -> [a] -> [b]
map f = undefined

reverse :: [a] -> [a]
reverse = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: (Integral i) => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: (Eq a) => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : xs) = case xs of
    [x] -> Just x
    (x : xs) -> safeLast xs

-- dec2int [1,9,9,2] = 1992
dec2int :: (Integral i) => [i] -> i
dec2int [] = 0
dec2int (x : xs) = x * 10 ^ length xs + dec2int xs
