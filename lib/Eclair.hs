module Eclair
  ( compileRA
  , compileEIR
  , compileLLVM
  , compile
  , run
  ) where

import Protolude hiding (swap)
import qualified Data.Map as M
import Eclair.Lowering.AST
import Eclair.Lowering.RA
import Eclair.Lowering.EIR
import Eclair.Parser
import Eclair.Syntax
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.Interpreter
import Eclair.TypeSystem
import LLVM.AST (Module)


type Relation = Id
type RA = RA.RA
type EIR = EIR.EIR

compileRA :: FilePath -> IO (Either ParseError RA)
compileRA path =
  map compileToRA <$> parseFile path

compileEIR :: FilePath -> IO (Either ParseError EIR)
compileEIR path = do
  parseResult <- parseFile path
  case parseResult of
    Left err -> pure $ Left err
    Right ast -> do
      let typeInfo = getTypeInfo ast
          ra = compileToRA ast
      pure $ Right $ compileToEIR typeInfo ra

compileLLVM :: FilePath -> IO (Either ParseError Module)
compileLLVM path =
  traverse compileToLLVM =<< compileEIR path

compile :: FilePath -> IO (Either ParseError Module)
compile = compileLLVM

run :: FilePath -> IO (M.Map Relation [[Number]])
run path = do
  raResult <- compileRA path
  case raResult of
    Left err -> do
      printParseError err
      panic "Failed to interpret path."
    Right ra -> do
      interpretRA ra
