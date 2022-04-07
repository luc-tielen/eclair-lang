module Eclair ( compile, run ) where

import Protolude hiding (swap)
import qualified Data.Map as M
import Eclair.Lowering.AST
import Eclair.Lowering.RA
import Eclair.Lowering.EIR
import Eclair.Parser
import Eclair.EIR.IR
import Eclair.RA.Interpreter
import Eclair.Syntax
import Eclair.TypeSystem
import LLVM.AST (Module)


compile :: FilePath -> IO (Either ParseError Module)
compile path = do
  parseResult <- parseFile path
  case parseResult of
    Left err -> pure $ Left err
    Right ast -> do
      let typeInfo = getTypeInfo ast
          ra = compileRA ast
          eir = compileToEIR typeInfo ra
      llvm <- compileToLLVM eir
      pure $ Right llvm

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
