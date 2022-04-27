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


type Relation = RA.Relation
type RA = RA.RA
type EIR = EIR.EIR

data EclairError
  = ParseErr ParseError
  | TypeErr [TS.TypeError]

-- TODO: refactor all these helper functions to be more composable

parse :: FilePath -> IO (Either EclairError AST)
parse path =
  map (mapError ParseErr) $ parseFile path

typeCheck :: FilePath -> IO (Either EclairError TS.TypeInfo)
typeCheck path = do
  parseResult <- parse path
  pure $ do
    ast <- parseResult
    mapError TypeErr $ TS.typeCheck ast

compileRA :: FilePath -> IO (Either EclairError RA)
compileRA path = do
  parseResult <- parse path
  pure $ do
    ast <- parseResult
    typeInfo <- mapError TypeErr $ TS.typeCheck ast
    pure $ compileToRA ast

compileEIR :: FilePath -> IO (Either EclairError EIR)
compileEIR path = do
  parseResult <- parse path
  pure $ do
    ast <- parseResult
    typeInfo <- mapError TypeErr $ TS.typeCheck ast
    let ra = compileToRA ast
    pure $ compileToEIR typeInfo ra

compileLLVM :: FilePath -> IO (Either EclairError Module)
compileLLVM path =
  traverse compileToLLVM =<< compileEIR path

compile :: FilePath -> IO (Either EclairError Module)
compile = compileLLVM

run :: FilePath -> IO (M.Map Relation [[Number]])
run path = do
  raResult <- compileRA path
  case raResult of
    Left (ParseErr err) -> do
      printParseError err
      panic "Failed to parse file."
    Left (TypeErr errs) -> do
      print errs
      panic "Failed to type-check file."
    Right ra -> do
      interpretRA ra

mapError :: (a -> c) -> Either a b -> Either c b
mapError f = \case
  Left a -> Left $ f a
  Right b -> Right b

