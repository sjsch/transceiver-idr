module Control.Transceiver.Syntax.Char

import Control.Transceiver.Syntax
import Control.Transceiver.Syntax.Combinators

%access export

digit : Chain s Char => Syn s Int
digit = emap ((+ -48) . ord) (chr . (+48)) c
  where c = satisfies isDigit
