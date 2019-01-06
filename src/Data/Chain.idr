module Data.Chain
%access public export

interface Chain s t | s where
  emptyChain : s
  appendChain : s -> t -> s
  unconsChain : s -> Maybe (t, s)

Chain (List a) a where
  emptyChain = Nil
  appendChain l x = l ++ [x]
  unconsChain [] = Nothing
  unconsChain (x::xs) = Just (x, xs)

Chain String Char where
  emptyChain = ""
  appendChain s x = s ++ singleton x
  unconsChain "" = Nothing
  unconsChain s = Just (strHead s, strTail s)
