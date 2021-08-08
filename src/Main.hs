module Main (main) where

import Protolude
import Eclair
import Eclair.RA.Printer


main :: IO ()
main =
  compile "tests/fixtures/codegen/index_selection.dl" >>= \case
    Left _ -> panic "Failed to compile to RA"
    Right ra -> putStrLn $ printRA ra
