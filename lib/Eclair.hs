module Eclair ( compile, run ) where

import Protolude
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax
import Eclair.Parser
import Eclair.RA.Codegen
import Eclair.RA.IR
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T


compileRA :: AST -> RA
compileRA ast = RAModule $ concatMap processDecls sortedDecls where
  sortedDecls = scc ast

  processDecls :: [AST] -> [RA]
  processDecls = \case
    [Atom name values] -> runCodegen $
      emit $ project name (LitTerm <$> values ^.. folded . _Lit)
    [Rule name args clauses] ->
      let terms = map toTerm args
          clauses' = map toClause clauses
      in runCodegen $ processSingleRule name terms clauses'
    rules ->  -- TODO: other cases!
      []
  processSingleRule name terms clauses = do
    -- TODO handle recursive rules..
    -- TODO: handle case where last isn't an atom (not possible yet, but it will be later)
    let (AtomClause cName cTerms : rest) = reverse clauses
    emit $ do
      flip (foldl' processRuleClause) rest $
        search cName cTerms $
          project name terms
  processRuleClause inner = \case
    AtomClause name terms -> search name terms inner

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
