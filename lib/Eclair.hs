module Eclair ( compile, run ) where

import Protolude hiding (swap)
import qualified Data.Map as M
import Eclair.Lowering.AST
import Eclair.Parser
import Eclair.RA.IR
import Eclair.RA.IndexSelection
import Eclair.RA.Interpreter
import Eclair.Syntax


compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  parseResult <- parseFile path
  case map compileRA parseResult of
    Left err -> pure $ Left err
    Right ra -> do
      let getIndexForSearch = getIndexForSearchInProgram ra
      -- TODO: use helper function in btree impl
      pure $ Right ra

run :: FilePath -> IO (M.Map Relation [[Number]])
run path = compile path >>= \case
  Left err -> do
    printParseError err
    panic "Failed to interpret path."
  Right ast -> do
    interpretRA ast

