module Control.Transceiver.Syntax.Combinators

import Data.Vect

import Control.Transceiver.Syntax

%access export

satisfies : Chain s t => (t -> Bool) -> Syn s t
satisfies p = MkSyn (MkParser f) printToken
  where
    f s = case unconsChain s of
      Just (a, s') => if p a then Just (a, s') else Nothing
      Nothing => Nothing

string : (Eq t, Chain s t) => List t -> Syn s ()
string [] = combineId ()
string (x::xs) = emap f g $ combine (satisfies (==x)) (string xs)
  where f (_, ()) = ()
        g () = (x, ())

repeatE : Combinable e => e a -> (n : Nat) -> e (Vect n a)
repeatE _ Z = combineId []
repeatE e (S n) = emap f g $ combine e (repeatE e n)
  where f (x, xs) = x::xs
        g (x::xs) = (x, xs)
