module Control.Transceiver.Syntax

import public Control.Transceiver.Parser
import public Control.Transceiver.Printer

import public Data.Chain
import public Data.Functor.Exp
import public Data.Functor.Contravariant
import public Data.Functor.Product

%access public export

Syn : Type -> Type -> Type
Syn s a = Product (Parser s) (Printer s) a

MkSyn : Parser s a -> Printer s a -> Syn s a
MkSyn = MkProduct

token : Chain s t => Syn s t
token = MkSyn parseToken printToken

eof : Chain s t => Syn s ()
eof = MkSyn parseEof printEof

runParser : Syn s a -> s -> Maybe (a, s)
runParser (MkProduct (MkParser p) _) = p

runPrinter : Syn s a -> a -> s -> s
runPrinter (MkProduct _ (MkPrinter p)) = p
