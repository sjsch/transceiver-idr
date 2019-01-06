module Exp
%access public export

interface Exp (f : Type -> Type) where
  emap : (a -> b) -> (b -> a) -> f a -> f b

interface Exp f => Combinable (f : Type -> Type) where
  combine : f a -> f b -> f (a, b)
  combineId : a -> f a

interface Combinable f => Pickable (f : Type -> Type) where
  pick : f a -> f b -> f (Either a b)
  pickId : f Void
