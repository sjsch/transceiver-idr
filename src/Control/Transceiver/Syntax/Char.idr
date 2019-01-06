module Char

import Control.Transceiver.Syntax

import Data.Fin

%access export

satisfies : Chain s t => (t -> Bool) -> Syn s t
satisfies p = MkSyn (MkParser f) printToken
  where
    f s = case unconsChain s of
      Just (a, s') => if p a then Just (a, s') else Nothing
      Nothing => Nothing

digit : Chain s Char => Syn s Int
digit = emap ((+ -48) . ord) (chr . (+48)) c
  where c = satisfies isDigit
