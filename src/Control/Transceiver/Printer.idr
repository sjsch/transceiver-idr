module Printer

import Data.Chain
import Data.Functor.Contravariant

%access public export

data Printer s a = MkPrinter (a -> s -> s)

Contravariant (Printer s) where
  contramap f (MkPrinter p) = MkPrinter $ \a => p (f a)

Divisible (Printer s) where
  divide (MkPrinter p) (MkPrinter q) = MkPrinter $
    \(a, b), s => q b (p a s)
  divideId = MkPrinter (const id)

Decidable (Printer s) where
  choose (MkPrinter p) (MkPrinter q) = MkPrinter $
    \a, s => case a of
      Left x => p x s
      Right x => q x s
  chooseId = MkPrinter absurd

printToken : Chain s t => Printer s t
printToken = MkPrinter (flip appendChain)

printEof : Chain s t => Printer s ()
printEof = divideId
