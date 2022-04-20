module Eclair.Pretty
  ( module Eclair.Pretty
  , module Prettyprinter
  , module Prettyprinter.Render.Text
  ) where

import Prettyprinter
import Prettyprinter.Render.Text


printDoc :: Pretty a => a -> Text
printDoc = renderStrict . layoutSmart defaultLayoutOptions . pretty

indentation :: Int
indentation = 2

interleaveWith :: Doc ann -> [Doc ann] -> Doc ann
interleaveWith d = hsep . punctuate d

withCommas :: [Doc ann] -> Doc ann
withCommas = interleaveWith comma

withAnds :: [Doc ann] -> Doc ann
withAnds = interleaveWith (space <> "and")

between :: Doc ann -> Doc ann -> Doc ann -> Doc ann
between begin end doc =
  begin <> doc <> end

