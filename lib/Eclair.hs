module Eclair
  ( parse
  , compileRA
  , compileEIR
  , compileLLVM
  , compile
  , run
  , EclairError(..)
  ) where

import qualified Data.Map as M
import Eclair.AST.Lower
import Eclair.RA.Lower
import Eclair.EIR.Lower
import Eclair.Parser
import Eclair.Id
import Eclair.AST.IR
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.Interpreter
import qualified Eclair.TypeSystem as TS
import LLVM.AST (Module)
import Control.Exception
import LLVM.Pretty


type Relation = RA.Relation
type RA = RA.RA
type EIR = EIR.EIR

data EclairError
  = ParseErr ParseError
  | TypeErr [TS.TypeError]
  deriving (Show, Exception)

-- TODO: refactor all these helper functions to be more composable

parse :: FilePath -> IO AST
parse path =
  parseFile path >>= either (throwIO . ParseErr) pure

typeCheck :: FilePath -> IO TS.TypeInfo
typeCheck path = do
  ast <- parse path
  either (throwIO . TypeErr) pure $ TS.typeCheck ast

compileRA :: FilePath -> IO RA
compileRA path = do
  ast <- parse path
  typeInfo <- typeCheck path  -- TODO don't re-parse
  pure $ compileToRA ast

compileEIR :: FilePath -> IO EIR
compileEIR path = do
  ra <- compileRA path
  typeInfo <- typeCheck path  -- TODO don't re-parse
  pure $ compileToEIR typeInfo ra

compileLLVM :: FilePath -> IO Module
compileLLVM path =
  compileToLLVM =<< compileEIR path

compile :: FilePath -> IO ()
compile path = handle handleErrors $ do
  llvmModule <- compileLLVM path
  putLTextLn $ ppllvm llvmModule

run :: FilePath -> IO (M.Map Relation [[Number]])
run path = do
  ra <- compileRA path
  interpretRA ra

handleErrors :: EclairError -> IO ()
handleErrors = \case
  ParseErr err -> do
    printParseError err
    panic "Failed to parse file."
  TypeErr errs -> do
    print errs
    panic "Failed to type-check file."
