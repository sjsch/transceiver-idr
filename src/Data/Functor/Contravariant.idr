module Contravariant
%access public export

interface Contravariant (f : Type -> Type) where
  contramap : (b -> a) -> f a -> f b

interface Contravariant f => Divisible (f : Type -> Type) where
  divide : f a -> f b -> f (a, b)
  divideId : f a

interface Divisible f => Decidable (f : Type -> Type) where
  choose : f a -> f b -> f (Either a b)
  chooseId : f Void
