{-# LANGUAGE GADTs #-}

module ExList where

import Data.Char qualified as C
import Data.List qualified as L
import Prelude (
  Bool (..),
  Char,
  Double,
  Enum (..),
  Eq (..),
  Float,
  Int,
  Integer,
  Integral (..),
  Num (..),
  Ord (..),
  String,
  curry,
  error,
  flip,
  not,
  otherwise,
  uncurry,
  undefined,
  ($),
  (&&),
  (.),
  (||),
 )
import Prelude qualified as P

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}

{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x : xs) = x
head _ = error "headless"

tail :: [a] -> [a]
tail (x : xs) = xs
tail _ = []

null :: [a] -> Bool
null [] = True
null _ = False

length :: (Integral i) => [a] -> i
length (x : xs) = 1 + length xs
length _ = 0

sum :: (Num a) => [a] -> a
sum (x : xs) = x + sum xs
sum _ = 0

product :: (Num a) => [a] -> a
product (x : xs) = x * product xs
product _ = 1

reverse :: [a] -> [a]
reverse (x : xs) = reverse xs ++ [x]
reverse _ = []

(++) :: [a] -> [a] -> [a]
[] ++ l = l
l ++ [] = l
(x : xs) ++ l = x : (xs ++ l)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

minimum :: (Ord a) => [a] -> a
minimum [] = error "Lista vazia"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: (Ord a) => [a] -> a
maximum [] = error "Lista vazia"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take _ [] = []
take 0 l = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 l = l
drop n (x : xs) = drop (n - 1) xs

-- pending use any
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = takeWhile p xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : dropWhile p xs

init :: [a] -> [a]
init [] = error "Lista vazia"
init [x] = []
init (x : xs) = x : init xs

-- inits
-- tails

-- subsequences

-- add typeclass
any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x : xs)
  | p x = True
  | otherwise = any p xs

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x : xs)
  | p x = all p xs
  | otherwise = False

-- add typeclass
and :: [Bool] -> Bool
and [] = True
and (b : bs)
  | b = and bs
  | otherwise = False

or :: [Bool] -> Bool
or [] = False
or (b : bs)
  | b = True
  | otherwise = or bs

-- concat :: [[a]] -> [a]

-- elem using the funciton 'any' above
elem :: (Eq a) => a -> [a] -> Bool
elem n l = any (n ==) l

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = elem' n xs

(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! i
  | (i <= 0) || i >= length (x : xs) = error "index inválido"
  | otherwise = xs !! (i - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "Lista vazia"
cycle l = l ++ cycle l

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0 = []
  | otherwise = x : replicate (n - 1) x

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
