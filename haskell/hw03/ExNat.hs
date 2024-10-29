{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!

import Data.Bits (Bits (xor))
import Prelude (
  Bool (..),
  Eq (..),
  Integral (..),
  Num (..),
  Ord (..),
  Show (..),
  error,
  not,
  otherwise,
  undefined,
  ($),
  (&&),
  (++),
  (.),
  (||),
 )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show O = "O"
  show (S n) = "S" ++ show n

instance Eq Nat where
  O == O = True
  (S n) == (S m) = n == m
  _ == _ = False

instance Ord Nat where
  O <= _ = True
  _ <= O = False
  (S n) <= (S m) = n <= m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min = undefined

  max = undefined

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S n) = n
pred O = O

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd n = not (even n)

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> (S m) = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
O <-> _ = O
n <-> O = n
(S n) <-> (S m) = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> O = O
n <*> (S m) = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> O = S O
n <^> (S m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> O = error "Dividiu por O"
n </> m =
  if n <= m
    then O
    else S ((n <-> m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> O = error "Dividiu por O"
n <%> m =
  if n <= m
    then n
    else S ((n <-> m) <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = n </> m == O

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n O = n
absDiff O n = n
absDiff (S n) (S m) = absDiff n m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = error "Não existe"
lo _ (S O) = O
lo n m =
  if n > m
    then O
    else S (lo n (m </> n))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: (Integral a) => a -> Nat
toNat n
  | n < 0 = error "Número negativo"
  | n == 0 = O
  | n > 0 = S (toNat (n - 1))

fromNat :: (Integral a) => Nat -> a
fromNat n
  | n == O = 0
  | S n > O = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger = toNat
