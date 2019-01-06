module Product

import Data.Functor.Contravariant
import Data.Functor.Exp

%access public export

data Product : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkProduct : f a -> g a -> Product f g a

(Functor f, Contravariant c) => Exp (Product f c) where
  emap f g (MkProduct x y) = MkProduct (map f x) (contramap g y)

(Applicative f, Divisible c) => Combinable (Product f c) where
  combine (MkProduct fx cx) (MkProduct fy cy) = MkProduct
    [| MkPair fx fy |]
    (divide cx cy)
  combineId x = MkProduct (pure x) divideId

(Alternative f, Decidable c) => Pickable (Product f c) where
  pick (MkProduct fx cx) (MkProduct fy cy ) = MkProduct
    ([| Left fx |] <|> [| Right fy |])
    (choose cx cy)
  pickId = MkProduct empty chooseId
