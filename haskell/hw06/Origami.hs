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
foldl :: undefined
foldl = undefined

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 o l = case l of
    [x] -> x
    (x : xs) -> x `o` (foldr1 o xs)

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: undefined
foldl1 = undefined

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
scanl = undefined

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = undefined

--
-- Define all of the following functions as folds:
--

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

and :: [Bool] -> Bool
and = undefined

or :: [Bool] -> Bool
or = undefined

minimum :: (Ord a) => [a] -> a
minimum = undefined

maximum :: (Ord a) => [a] -> a
maximum = undefined

length :: (Integral i) => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

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
