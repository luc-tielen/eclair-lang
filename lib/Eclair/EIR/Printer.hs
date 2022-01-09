module Eclair.EIR.Printer ( Pretty, printEIR ) where

import Protolude
import Prettyprinter
import Eclair.EIR.IR
import qualified Data.Text as T

printEIR :: EIR -> T.Text
printEIR = panic "Not implemented!"
