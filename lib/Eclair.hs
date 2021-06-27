module Eclair ( run ) where

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
      project name (LitTerm <$> values ^.. folded . _Lit)
    rules ->
      -- TODO!
      []
{-
    [Rule name args clauses] ->
      processSingleRule name args clauses
    -- TODO: which other cases?
    rules -> RAModule []
    -- TODO finish other cases
  processSingleRule name args clauses =
    -- TODO handle recursive rules that are not mutually recursive..
    let eqs = constraintsForRule name args clauses
        (c@(Atom cName values) : rest) = reverse clauses  -- TODO handle other cases (no atom at front)
        seed = Search cName (clauseConstraints eqs c) $
          Project name $ map (valueToRA eqs) args
     in foldl' (\raCode clause -> Search (clauseName clause) (clauseConstraints eqs clause) raCode) seed rest
-}
compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
