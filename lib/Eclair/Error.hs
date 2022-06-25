{-# LANGUAGE MultiParamTypeClasses #-}

module Eclair.Error
  ( EclairError(..)
  , handleErrors
  ) where

import Eclair.AST.Analysis
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Id
import Prettyprinter
import Prettyprinter.Render.Terminal
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec


data EclairError
  = ParseErr FilePath ParseError
  | TypeErr FilePath SpanMap [TypeError]
  | SemanticErr FilePath SpanMap SemanticErrors
  deriving (Show, Exception)

-- TODO refactor using an error reporting monad?

handleErrors :: EclairError -> IO ()
handleErrors = \case
  ParseErr file err -> do
    content <- readFile file
    let diagnostic = errorDiagnosticFromBundle Nothing "Failed to parse file" Nothing err
        diagnostic' = addFile diagnostic file content
     in renderError diagnostic'
  TypeErr file spanMap errs -> do
    content <- readFileText file
    let reports = map (typeErrorToReport file content spanMap) errs
        diagnostic = foldl' addReport def reports
        diagnostic' = addFile diagnostic file (toString content)
     in renderError diagnostic'
  SemanticErr file spanMap semanticErr -> do
    content <- readFileText file
    let reports = semanticErrorsToReports file content spanMap semanticErr
        diagnostic = foldl' addReport def reports
        diagnostic' = addFile diagnostic file (toString content)
     in renderError diagnostic'

-- TODO add more info
typeErrorToReport :: FilePath -> Text -> SpanMap -> TypeError -> Report Text
typeErrorToReport file fileContent spanMap = \case
  UnknownAtom atom ->
    err Nothing ("Failed to check type of unknown fact '" <> unId atom <> "'") [] []
  ArgCountMismatch atom expectedCount actualCount ->
    err Nothing ("Found an unexpected amount of arguments for fact '" <> unId atom <> "'") [] []
  DuplicateTypeDeclaration atom ->
    err Nothing ("Duplicate type declaration for fact '" <> unId atom <> "'") [] []

-- TODO finish
-- TODO helpful error message pointing to a getting started guide
emptyModuleToReport :: FilePath -> Text -> SpanMap -> EmptyModule -> Report Text
emptyModuleToReport file fileContent spanMap (EmptyModule nodeId) =
  err Nothing "Nothing to do for empty module, aborting" [] []

-- TODO finish
-- TODO explain what ungrounded means
ungroundedVarToReport :: FilePath -> Text -> SpanMap -> UngroundedVar -> Report Text
ungroundedVarToReport file fileContent spanMap (UngroundedVar nodeId var) =
  err Nothing ("Variable '" <> unId var <> "' is ungrounded") [] []

missingTypedefToReport :: FilePath -> Text -> SpanMap -> MissingTypedef -> Report Text
missingTypedefToReport file fileContent spanMap (MissingTypedef nodeId factName) =
  -- TODO refactor
  let span = lookupSpan spanMap nodeId
      srcLoc = sourceSpanToPosition file $ spanToSourceSpan file fileContent span
      markers = [(srcLoc, This $ "Could not find a type definition for '" <> unId factName <> "'.")]
      hints = ["You can solve this by adding a type definition for '" <> unId factName <> "'."]
  in err Nothing "Missing type definition" markers hints

-- TODO finish
-- TODO explain why it is not allowed
wildcardInFactToReport :: FilePath -> Text -> SpanMap -> WildcardInFact -> Report Text
wildcardInFactToReport file fileContent spanMap (WildcardInFact factNodeId factArgId pos) =
  err Nothing ("Found wildcard in top level fact") [] []

-- TODO finish
wildcardInRuleHeadToReport :: FilePath -> Text -> SpanMap -> WildcardInRuleHead -> Report Text
wildcardInRuleHeadToReport file fileContent spanMap (WildcardInRuleHead nodeId _ _) =
  err Nothing ("Found wildcard in the 'head' of a rule") [] []

-- TODO finish
wildcardInAssignmentToReport :: FilePath -> Text -> SpanMap -> WildcardInAssignment -> Report Text
wildcardInAssignmentToReport file fileContent spanMap (WildcardInAssignment nodeId _) =
  err Nothing ("Found a wildcard in an assignment") [] []

-- NOTE: pattern match is done this way to keep track of additional errors that need to be reported
semanticErrorsToReports :: FilePath -> Text -> SpanMap -> SemanticErrors -> [Report Text]
semanticErrorsToReports file fileContent spanMap e@(SemanticErrors _ _ _ _ _ _ _) =
  concat [ emptyModuleReports
         , ungroundedVarReports
         , missingTypedefReports
         , wildcardInFactReports
         , wildcardInRuleHeadReports
         , wildcardInAssignmentReports
         ]
  where
    getReportsFor :: (SemanticErrors -> Container a)
                  -> (FilePath -> Text -> SpanMap -> a -> Report Text)
                  -> [Report Text]
    getReportsFor f g = map (g file fileContent spanMap) (f e)
    emptyModuleReports = getReportsFor emptyModules emptyModuleToReport
    ungroundedVarReports = getReportsFor ungroundedVars ungroundedVarToReport
    missingTypedefReports = getReportsFor missingTypedefs missingTypedefToReport
    wildcardInFactReports = getReportsFor wildcardsInFacts wildcardInFactToReport
    wildcardInRuleHeadReports = getReportsFor wildcardsInRuleHeads wildcardInRuleHeadToReport
    wildcardInAssignmentReports = getReportsFor wildcardsInAssignments wildcardInAssignmentToReport

sourceSpanToPosition :: FilePath -> SourceSpan -> Position
sourceSpanToPosition file sourceSpan =
  let beginPos = beginSourceSpan sourceSpan
      endPos = endSourceSpan sourceSpan
      start = (sourcePosLine beginPos, sourcePosColumn beginPos)
      end = (sourcePosLine endPos, sourcePosColumn endPos)
   in Position start end file

renderError :: Diagnostic Text -> IO ()
renderError =
  printDiagnostic stderr useUnicode useColors tabSpaces style
  where
    useUnicode = True
    useColors = True
    tabSpaces = 2

style :: Style
style = reAnnotate style
  where
    style = \case
      ThisColor isError ->
        colorDull $ if isError then Red else Yellow
      MaybeColor ->
        color Magenta
      WhereColor ->
        colorDull Blue
      HintColor ->
        colorDull Cyan
      FileColor ->
        bold <> colorDull Green
      RuleColor ->
        bold <> color Black
      KindColor isError ->
        bold <> style (ThisColor isError)
      NoLineColor ->
        bold <> colorDull Magenta
      MarkerStyle st ->
        bold <> style st

instance HasHints Void msg where
  hints = const mempty
