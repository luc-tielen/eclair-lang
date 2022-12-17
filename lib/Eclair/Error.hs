{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclair.Error
  ( EclairError(..)
  , handleErrors
  ) where

import qualified Data.Map as M
import Data.List (partition)
import Eclair.AST.Analysis
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Id
import Prettyprinter
import Prettyprinter.Render.Terminal
import Error.Diagnose hiding (stderr)
import Error.Diagnose.Compat.Megaparsec
import  System.IO hiding (stderr, readFile)


data EclairError
  = ParseErr FilePath ParsingError
  | TypeErr FilePath SpanMap [TypeError]
  | SemanticErr FilePath SpanMap SemanticErrors
  deriving (Show, Exception)

-- TODO refactor using an error reporting monad?

handleErrors :: EclairError -> IO ()
handleErrors = \case
  ParseErr file' err' -> do
    case err' of
      FileNotFound {} ->
        hPutStrLn stderr $ "File not found: " <> file'
      ParsingError parseError -> do
        content <- readFile file'
        let diagnostic = errorDiagnosticFromBundle Nothing "Failed to parse file" Nothing parseError
            diagnostic' = addFile diagnostic file' content
         in renderError diagnostic'

  TypeErr file' spanMap errs -> do
    content <- readFileText file'
    let reports = map (typeErrorToReport file' content spanMap) errs
        diagnostic = foldl' addReport def reports
        diagnostic' = addFile diagnostic file' (toString content)
     in renderError diagnostic'

  SemanticErr file' spanMap semanticErr -> do
    content <- readFileText file'
    let reports = semanticErrorsToReports file' content spanMap semanticErr
        diagnostic = foldl' addReport def reports
        diagnostic' = addFile diagnostic file' (toString content)
     in renderError diagnostic'

typeErrorToReport :: FilePath -> Text -> SpanMap -> TypeError -> Report Text
typeErrorToReport file' fileContent spanMap = \case
  UnknownAtom nodeId factName ->
    let srcLoc = getSourcePos file' fileContent spanMap nodeId
        title = "Missing type definition"
        markers = [(srcLoc, This $ "Could not find a type definition for '" <> unId factName <> "'.")]
        hints' = [Hint $ "You can solve this by adding a type definition for '" <> unId factName <> "'."]
    in Err Nothing title markers hints'

  ArgCountMismatch factName (typedefNodeId, expectedCount) (factNodeId', actualCount) ->
    let title = "Found an unexpected amount of arguments for fact '" <> unId factName <> "'"
        expectedSrcLoc = getSourcePos file' fileContent spanMap typedefNodeId
        actualSrcLoc = getSourcePos file' fileContent spanMap factNodeId'
        markers = [ (actualSrcLoc, This $ show actualCount <> pluralize actualCount " argument is" " arguments are" <> " provided here.")
                  , (expectedSrcLoc, Where $ "'" <> unId factName <> "' is defined with " <> show expectedCount <>
                    pluralize expectedCount " argument." " arguments.")
                  ]
        hints' = [Hint $ "You can solve this by passing exactly " <> show expectedCount <> " arguments to '" <> unId factName <> "'."]
    in Err Nothing title markers hints'

  DuplicateTypeDeclaration factName decls ->
    Err Nothing title (mainMarker:markers) hints'
    where
      title = "Multiple type declarations for fact '" <> unId factName <> "'"
      mainMarker =
        let nodeId = fst $ head decls
            srcLoc = getSourcePos file' fileContent spanMap nodeId
         in (srcLoc, This $ "'" <> unId factName <> "' is originally defined here.")
      markers = tail decls & toList & map (\(nodeId, _tys) ->
        let srcLoc = getSourcePos file' fileContent spanMap nodeId
         in (srcLoc, Where $ "'" <> unId factName <> "' is re-defined here."))
      hints' = [Hint $ "You can solve this by removing the duplicate definitions for '" <> unId factName <> "'."]

  TypeMismatch nodeId actualTy expectedTy ctx ->
    Err Nothing title markers hints'
      where
        title = "Type mismatch"
        srcLoc = getSourcePos file' fileContent spanMap nodeId
        lastMarker = (srcLoc, This $ show (length ctx + 1) <> ") Expected this to be of type " <> renderType expectedTy <> ","
                    <> " but it actually has type " <> renderType actualTy <> ".")
        markers = zipWith renderDeduction markerTypes (toList ctx) ++ [lastMarker]
        markerTypes = zip [1..] $ repeat Where
        hints' = []  -- Can we even give a meaningful error here? Look in type env? (if a var is used)

  UnificationFailure _ _ ctx ->
    Err Nothing title markers hints'
      where
        title = "Type unification failure"
        markerTypes = markersForTypeError ctx
        markers = zipWith renderDeduction markerTypes (toList ctx)
        hints' = []  -- What can we even give as a hint here? That it is a logical error?

  HoleFound nodeId ctx holeTy typeEnv ->
    Err Nothing title markers hints'
      where
        srcLoc = getSourcePos file' fileContent spanMap nodeId
        title = "Found hole"
        markerTypes =  zip [1..] $ repeat Where
        deductions = zipWith renderDeduction markerTypes (toList ctx)
        markers = deductions <>
          [(srcLoc, This $ show (length deductions + 1) <> ") Found hole with type " <> renderType holeTy <> ".")]
        typeEntries =
          typeEnv
            & M.mapWithKey (\var ty -> (ty, renderBinding var ty))
            & toList
        (candidates, others) = partition (\(entryTy, _) -> entryTy == holeTy) typeEntries
        hints' =
          map (Hint . (("Possible candidate: " <>) . snd)) candidates <>
          if null others
            then []
            else [Hint "Other variables include:"] <> map (Hint . snd) others
        renderBinding var ty =
          unId var <> " :: " <> renderType ty
  where
    renderType ty =
      let userFacingType = case ty of
            U32 -> "u32"
            Str -> "string"
            TUnknown x -> "t" <> show x
      in "'" <> userFacingType <> "'"

    markersForTypeError ctx =
      zip [1..] $ replicate (length ctx - 1) Where ++ [This]

    renderDeduction :: (Int, Text -> Marker a) -> Context -> (Position, Marker a)
    renderDeduction (i, mkMarker) = \case
      WhileChecking nodeId ->
        let srcLoc = getSourcePos file' fileContent spanMap nodeId
          in (srcLoc, mkMarker $ show i <> ") While checking the type of this..")
      WhileInferring nodeId ->
        let srcLoc = getSourcePos file' fileContent spanMap nodeId
          in (srcLoc, mkMarker $ show i <> ") While inferring the type of this..")
      WhileUnifying nodeId ->
        let srcLoc = getSourcePos file' fileContent spanMap nodeId
          in (srcLoc, mkMarker $ show i <> ") While unifying these types..")

emptyModuleToReport :: FilePath -> Text -> SpanMap -> EmptyModule -> Report Text
emptyModuleToReport _ _ _ (EmptyModule _) =
  let title = "Empty module"
      markers = []
      -- TODO: write out some more getting started docs
      hints' = ["See https://github.com/luc-tielen/eclair-lang#example-code for some example code! :)"]
   in Err Nothing title markers hints'

variableInFactToReport :: FilePath -> Text -> SpanMap -> VariableInFact -> Report Text
variableInFactToReport file' fileContent spanMap (VariableInFact nodeId _) =
  let srcLoc = getSourcePos file' fileContent spanMap nodeId
      title = "Variable in top level fact"
      markers = [(srcLoc, This "Only constants are allowed in facts.")]
      hints' = ["You can solve this by replacing the variable with a constant."]
   in Err Nothing title markers hints'

ungroundedVarToReport :: FilePath -> Text -> SpanMap -> UngroundedVar -> Report Text
ungroundedVarToReport file' fileContent spanMap (UngroundedVar ruleNodeId varNodeId var) =
  let title = "Ungrounded variable"
      srcLocRule = getSourcePos file' fileContent spanMap ruleNodeId
      srcLocVar = getSourcePos file' fileContent spanMap varNodeId
      markers = [ (srcLocVar, This $ "The variable '" <> unId var <> "' is ungrounded, meaning it is not directly bound as an argument to a relation.")
                , (srcLocRule, Where $ "This rule contains no clauses that refer to '" <> unId var <> "'.")
                ]
      hints' = [Hint $ "Use the variable '" <> unId var <> "' as an argument in another clause in the same rule."]
   in Err Nothing title markers hints'

wildcardInFactToReport :: FilePath -> Text -> SpanMap -> WildcardInFact -> Report Text
wildcardInFactToReport file' fileContent spanMap (WildcardInFact factNodeId' factArgId' _pos) =
  let title = "Wildcard in top level fact"
      srcLocFact = getSourcePos file' fileContent spanMap factNodeId'
      srcLocArg = getSourcePos file' fileContent spanMap factArgId'
      markers = [ (srcLocArg, This "Wildcard found.")
                , (srcLocFact, Where "A top level fact only supports constants.\nVariables or wildcards are not allowed.")
                ]
      hints' = ["Replace the wildcard with a constant."]
   in Err Nothing title markers hints'

wildcardInRuleHeadToReport :: FilePath -> Text -> SpanMap -> WildcardInRuleHead -> Report Text
wildcardInRuleHeadToReport file' fileContent spanMap (WildcardInRuleHead ruleNodeId ruleArgId _pos) =
  let title = "Wildcard in 'head' of rule"
      srcLocRule = getSourcePos file' fileContent spanMap ruleNodeId
      srcLocArg = getSourcePos file' fileContent spanMap ruleArgId
      markers = [ (srcLocArg, This "Wildcard found.")
                , (srcLocRule, Where "Only constants and variables are allowed in the head of a rule.\nWildcards are not allowed.")
                ]
      hints' = ["Replace the wildcard with a constant or a variable."]
   in Err Nothing title markers hints'

wildcardInAssignmentToReport :: FilePath -> Text -> SpanMap -> WildcardInAssignment -> Report Text
wildcardInAssignmentToReport file' fileContent spanMap (WildcardInAssignment assignNodeId wildcardNodeId') =
  let title = "Found wildcard in equality constraint"
      srcLocWildcard = getSourcePos file' fileContent spanMap wildcardNodeId'
      srcLocAssign = getSourcePos file' fileContent spanMap assignNodeId
      markers = [ (srcLocWildcard, This "Wildcard found.")
                , (srcLocAssign, Where "Only constants and variables are allowed in an equality constraint.")
                ]
      hints' = ["This statement can be removed since it has no effect."]
   in Err Nothing title markers hints'


deadInternalRelationToReport :: FilePath -> Text -> SpanMap -> DeadInternalRelation -> Report Text
deadInternalRelationToReport file' fileContent spanMap (DeadInternalRelation nodeId r) =
  let title = "Dead internal relation"
      srcLoc = getSourcePos file' fileContent spanMap nodeId
      markers = [(srcLoc, This $ "The internal rule '" <> unId r <> "' has no facts or rules defined and will never produce results.")]
      hints' = [ Hint "This might indicate a logic error in your code."
               , Hint "Remove this rule if it is no longer needed."
               , Hint "Add 'input' to the declaration to indicate this rule is an input."
               ]
   in Err Nothing title markers hints'

noOutputRelationsToReport :: FilePath -> Text -> SpanMap -> NoOutputRelation -> Report Text
noOutputRelationsToReport file' _ _ (NoOutputRelation _) =
  let title = "No output relations found"
      markers = [(startOfFile file', This "This module does not produce any results")]
      hints' = [ Hint "Add an 'output' qualifier to one of the relations defined in this module." ]
  in Err Nothing title markers hints'

-- NOTE: pattern match is done this way to keep track of additional errors that need to be reported
{-# ANN semanticErrorsToReports ("HLint: ignore Use record patterns" :: String) #-}
semanticErrorsToReports :: FilePath -> Text -> SpanMap -> SemanticErrors -> [Report Text]
semanticErrorsToReports file' fileContent spanMap e@(SemanticErrors _ _ _ _ _ _ _ _) =
  concat [ emptyModuleReports
         , ungroundedVarReports
         , variableInFactReports
         , wildcardInFactReports
         , wildcardInRuleHeadReports
         , wildcardInAssignmentReports
         , deadInternalRelationReports
         , noOutputReports
         ]
  where
    getReportsFor :: (SemanticErrors -> Container a)
                  -> (FilePath -> Text -> SpanMap -> a -> Report Text)
                  -> [Report Text]
    getReportsFor f g = map (g file' fileContent spanMap) (f e)
    emptyModuleReports = getReportsFor emptyModules emptyModuleToReport
    ungroundedVarReports = getReportsFor ungroundedVars ungroundedVarToReport
    variableInFactReports = getReportsFor variablesInFacts variableInFactToReport
    wildcardInFactReports = getReportsFor wildcardsInFacts wildcardInFactToReport
    wildcardInRuleHeadReports = getReportsFor wildcardsInRuleHeads wildcardInRuleHeadToReport
    wildcardInAssignmentReports = getReportsFor wildcardsInAssignments wildcardInAssignmentToReport
    deadInternalRelationReports = getReportsFor deadInternalRelations deadInternalRelationToReport
    noOutputReports = getReportsFor noOutputRelations noOutputRelationsToReport

pluralize :: Int -> Text -> Text -> Text
pluralize count singular plural' =
  if count == 1 then singular else plural'

getSourcePos :: FilePath -> Text -> SpanMap -> NodeId -> Position
getSourcePos file' fileContent spanMap nodeId =
  let span' = lookupSpan spanMap nodeId
   in sourceSpanToPosition $ spanToSourceSpan file' fileContent span'

sourceSpanToPosition :: SourceSpan -> Position
sourceSpanToPosition sourceSpan =
  let beginPos' = sourceSpanBegin sourceSpan
      endPos' = sourceSpanEnd sourceSpan
      start = (sourcePosLine beginPos', sourcePosColumn beginPos')
      end' = (sourcePosLine endPos', sourcePosColumn endPos')
   in Position start end' (sourceSpanFile sourceSpan)

renderError :: Diagnostic Text -> IO ()
renderError txt = do
  useColorEnvVar <- fromMaybe "1" <$> lookupEnv "ECLAIR_USE_COLOR"
  let useColors = useColorEnvVar /= "0"
  printDiagnostic stderr useUnicode useColors tabSpaces style txt
  where
    useUnicode = True
    tabSpaces = 2

startOfFile :: FilePath -> Position
startOfFile = Position (1, 1) (1, 2)

style :: Style
style = reAnnotate style'
  where
    style' = \case
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
        bold <> style' (ThisColor isError)
      NoLineColor ->
        bold <> colorDull Magenta
      MarkerStyle st ->
        bold <> style' st
      CodeStyle ->
        color White

instance HasHints CustomParseErr msg where
  hints = const mempty
