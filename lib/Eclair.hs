module Eclair ( compile, run ) where

import Protolude hiding (swap)
import qualified Data.Map as M
import Eclair.Lowering.AST
import Eclair.Lowering.RA
import Eclair.Parser
import Eclair.EIR.IR
import Eclair.RA.Interpreter
import Eclair.Syntax
import Eclair.TypeSystem


compile :: FilePath -> IO (Either ParseError EIR)
compile path = do
  parseResult <- parseFile path
  case parseResult of
    Left err -> pure $ Left err
    Right ast -> do
      let typeInfo = getTypeInfo ast
          ra = compileRA ast
          eir = compileToEIR typeInfo ra
      pure $ Right eir -- TODO other return value?

-- TODO: refactor to use compile
run :: FilePath -> IO (M.Map Relation [[Number]])
run path = do
  parseResult <- parseFile path
  case parseResult of
    Left err -> do
      printParseError err
      panic "Failed to interpret path."
    Right ast -> do
      let typeInfo = getTypeInfo ast
          ra = compileRA ast
      interpretRA ra
