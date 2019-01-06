module Parser

import Data.Chain

%access public export

data Parser s a = MkParser (s -> Maybe (a, s))

Functor (Parser s) where
  map f (MkParser p) = MkParser $ \s => do
    (p', s') <- p s
    pure (f p', s')

Applicative (Parser s) where
  (MkParser f) <*> (MkParser p) = MkParser $ \s => do
    (f', s') <- f s
    (p', s'') <- (p s')
    pure (f' p', s'')
  pure a = MkParser $ \s => Just (a, s)

Alternative (Parser s) where
  (MkParser p) <|> (MkParser q) = MkParser $ \s =>
    p s <|> q s
  empty = MkParser (const Nothing)

parseToken : Chain s t => Parser s t
parseToken = MkParser unconsChain

parseEof : Chain s t => Parser s ()
parseEof = MkParser $ \s => case unconsChain s of
  Just _ => Nothing
  Nothing => Just ((), emptyChain)
