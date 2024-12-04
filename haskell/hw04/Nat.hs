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
  (-),
  (^),
 )

data Nat = O | S Nat

-- abbrevs (syntactic sugar)
one, two, three, four, five, six, seven, eight, nine, ten :: Nat
one = S O
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine

instance Show Nat where
  show O = "0"
  show (S O) = "1"
  show (S (S O)) = "2"
  show (S (S (S O))) = "3"
  show (S (S (S (S O)))) = "4"
  show (S (S (S (S (S O))))) = "5"
  show (S (S (S (S (S (S O)))))) = "6"
  show (S (S (S (S (S (S (S O))))))) = "7"
  show (S (S (S (S (S (S (S (S O)))))))) = "8"
  show (S (S (S (S (S (S (S (S (S O))))))))) = "9"
  show n = show (quot (n, ten)) ++ show (rem (n, ten))

instance Eq Nat where
  O == O = True
  (S n) == (S m) = n == m
  _ == _ = False

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
fib (S (S n)) = fib (S n) + fib n
fib n = n

min :: (Nat, Nat) -> Nat
min (O, _) = O
min (_, O) = O
min (S n, S m) = S (min (n, m))

max :: (Nat, Nat) -> Nat
max (O, n) = n
max (n, O) = n
max (S n, S m) = S (max (n, m))

-- Auxiliar

leq :: (Nat, Nat) -> Bool
leq (O, _) = True
leq (_, O) = False
leq (S n, S m) = leq (n, m)

ifthenelse :: Bool -> a -> a -> a
ifthenelse True x _ = x
ifthenelse False _ y = y

(-) :: Nat -> Nat -> Nat
O - _ = O
n - O = n
(S n) - (S m) = n - m

quot :: (Nat, Nat) -> Nat
quot (_, O) = error "Dividiu por 0"
quot (n, m) = ifthenelse (leq (n, m)) O (S (quot (n - m, m)))

rem :: (Nat, Nat) -> Nat
rem (_, O) = error "Dividiu por 0"
rem (n, m) = ifthenelse (leq (n, m)) n (rem (n - m, m))

div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = error "Dividiu por 0"
div (n, m) = (quot (n, m), rem (n, m))

gcd :: (Nat, Nat) -> Nat
gcd (O, O) = error "Infinity"
gcd (n, O) = n
gcd (n, m) = gcd (max (n, m) - min (n, m), min (n, m))

lcm :: (Nat, Nat) -> Nat
lcm (_, O) = O
lcm (O, _) = O
lcm (n, m) = quot (n * m, gcd (n, m))
