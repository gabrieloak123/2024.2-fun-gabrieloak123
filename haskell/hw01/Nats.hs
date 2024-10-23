module Nat where

import Prelude hiding (
  div,
  double,
  fact,
  fib,
  gcd,
  lcm,
  max,
  min,
  pred,
  quot,
  rem,
  (*),
  (+),
  (^),
 )

data Nat = O | S Nat
  deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n * m + n

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ S m = n ^ m * m

double :: Nat -> Nat
double = (* S (S O))

pred :: Nat -> Nat
pred (S n) = n
pred O = O

fact :: Nat -> Nat
fact (S n) = S n * fact n
fact O = S O

fib :: Nat -> Nat
fib n = n
fib (S (S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min O n = O
min n O = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S n) (S m) = S (max n m)

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

ifthenelse :: Bool -> a -> a -> a
ifthenelse True x _ = x
ifthenelse False _ y = y

quot :: Nat -> Nat -> Nat
quot _ O = error "Dividiu por 0"
quot n m = ifthenelse (leq n m) O (S (quot (n - m m)))
