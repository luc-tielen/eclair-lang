{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclair.Error
  ( EclairError(..)
  , Issue(..)
  , Location(..)
  , Pos(..)
  , handleErrorsCLI
  , errorToIssues
  , renderIssueMessage
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Megaparsec as P
import Data.List (partition)
import Eclair.AST.Analysis
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Id
import Prettyprinter
import Prettyprinter.Render.Terminal
import Error.Diagnose hiding (stderr)
import Error.Diagnose.Compat.Megaparsec


data EclairError
  = ParseErr FilePath ParsingError
  | TypeErr FilePath SpanMap [TypeError]
  | SemanticErr FilePath SpanMap SemanticErrors
  deriving (Show, Exception)

-- TODO refactor using an error reporting monad?

-- Handle errors when running in the CLI.
handleErrorsCLI :: EclairError -> IO ()
handleErrorsCLI e = do
  useColorEnvVar <- fromMaybe "1" <$> lookupEnv "ECLAIR_USE_COLOR"
  let useColors = if useColorEnvVar /= "0"
                    then Just UseColor
                    else Nothing
  hPutDoc stderr =<< errToDoc useColors e
  where
    errToDoc useColor = \case
      ParseErr file' err' -> do
        case err' of
          FileNotFound {} ->
            pure $ "File not found: " <> pretty file'
          ParsingError parseError -> do
            content <- toString <$> readFileText file'
            let diagnostic = errorDiagnosticFromBundle Nothing "Failed to parse file" Nothing parseError
                diagnostic' = addFile diagnostic file' content
             in pure $ prettyError useColor diagnostic'

      TypeErr file' spanMap errs -> do
        content <- readFileText file'
        let reports = map (typeErrorToReport file' content spanMap) errs
            diagnostic = foldl' addReport def reports
            diagnostic' = addFile diagnostic file' (toString content)
         in pure $ prettyError useColor diagnostic'

      SemanticErr file' spanMap semanticErr -> do
        content <- readFileText file'
        let reports = map fst $ semanticErrorsToReportsWithLocations file' content spanMap semanticErr
            diagnostic = foldl' addReport def reports
            diagnostic' = addFile diagnostic file' (toString content)
         in pure $ prettyError useColor diagnostic'


-- A single position in the code. 0-based!
data Pos
  = Pos
  { posLine :: {-# UNPACK #-} !Word32
  , posColumn :: {-# UNPACK #-} !Word32
  }

-- Actual location in the code (a range).
-- Contains the file, start and end of the position.
data Location
  = Location
  { locationFile :: FilePath
  , locationStart :: {-# UNPACK #-} !Pos
  , locationEnd :: {-# UNPACK #-} !Pos
  }

-- A helper type for referring to an issue at a location.
data Issue
  = Issue
  { issueMessage :: Report Text
  , issueLocation :: Location
  }

renderIssueMessage :: FilePath -> Text -> Issue -> Text
renderIssueMessage file' content issue =
  let report = addReport def $ issueMessage issue
      diagnostic = addFile report file' (toString content)
      doc = unAnnotate $ prettyError Nothing diagnostic
   in renderStrict . layoutSmart defaultLayoutOptions $ doc

-- A helper function that can be used from the LSP. Splits all errors into
-- separate issues for most flexibility and fine-grained reporting.
errorToIssues :: (FilePath -> IO Text) -> EclairError -> IO [Issue]
errorToIssues readTextFile = \case
  ParseErr file' err' -> do
    case err' of
      FileNotFound {} -> do
        let errMessage = "File not found: " <> toText file'
            report = Err Nothing errMessage [] []
            noLocationInfo = Location file' (Pos 0 0) (Pos 0 0)
        pure $ one $ Issue report noLocationInfo

      ParsingError parseError -> do
        let reportsWithLocs = errReportsWithLocationsFromBundle "Failed to parse file" parseError
        pure $ map (uncurry Issue) reportsWithLocs

  TypeErr file' spanMap errs -> do
    content <- readTextFile file'
    let toReport = typeErrorToReport file' content spanMap
        toLocation = positionToLocation . mainErrorPosition file' content spanMap
    pure $ map (uncurry Issue . (toReport &&& toLocation)) errs

  SemanticErr file' spanMap semanticErrs -> do
    content <- readTextFile file'
    let reportsWithLocs = semanticErrorsToReportsWithLocations file' content spanMap semanticErrs
    pure $ map (uncurry Issue) reportsWithLocs


typeErrorToReport :: FilePath -> Text -> SpanMap -> TypeError -> Report Text
typeErrorToReport file' fileContent spanMap e = case e of
  UnknownAtom _ factName ->
    let srcLoc = mainErrorPosition file' fileContent spanMap e
        title = "Missing type definition"
        markers = [(srcLoc, This $ "Could not find a type definition for '" <> unId factName <> "'.")]
        hints' = [Hint $ "You can solve this by adding a type definition for '" <> unId factName <> "'."]
    in Err Nothing title markers hints'

  ArgCountMismatch factName (typedefNodeId, expectedCount) (_, actualCount) ->
    let title = "Found an unexpected amount of arguments for fact '" <> unId factName <> "'"
        expectedSrcLoc = getSourcePos file' fileContent spanMap typedefNodeId
        actualSrcLoc = mainErrorPosition file' fileContent spanMap e
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
        let srcLoc = mainErrorPosition file' fileContent spanMap e
         in (srcLoc, This $ "'" <> unId factName <> "' is originally defined here.")
      markers = tail decls & toList & map (\(nodeId, _tys) ->
        let srcLoc = getSourcePos file' fileContent spanMap nodeId
         in (srcLoc, Where $ "'" <> unId factName <> "' is re-defined here."))
      hints' = [Hint $ "You can solve this by removing the duplicate definitions for '" <> unId factName <> "'."]

  TypeMismatch _ actualTy expectedTy ctx ->
    Err Nothing title markers hints'
      where
        title = "Type mismatch"
        srcLoc = mainErrorPosition file' fileContent spanMap e
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

  HoleFound _ ctx holeTy typeEnv ->
    Err Nothing title markers hints'
      where
        srcLoc = mainErrorPosition file' fileContent spanMap e
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
variableInFactToReport file' fileContent spanMap e =
  let srcLoc = mainErrorPosition file' fileContent spanMap e
      title = "Variable in top level fact"
      markers = [(srcLoc, This "Only constants are allowed in facts.")]
      hints' = ["You can solve this by replacing the variable with a constant."]
   in Err Nothing title markers hints'

ungroundedVarToReport :: FilePath -> Text -> SpanMap -> UngroundedVar -> Report Text
ungroundedVarToReport file' fileContent spanMap e@(UngroundedVar ruleNodeId _ var) =
  let title = "Ungrounded variable"
      srcLocRule = getSourcePos file' fileContent spanMap ruleNodeId
      srcLocVar = mainErrorPosition file' fileContent spanMap e
      markers = [ (srcLocVar, This $ "The variable '" <> unId var <> "' is ungrounded, meaning it is not directly bound as an argument to a relation.")
                , (srcLocRule, Where $ "This rule contains no clauses that refer to '" <> unId var <> "'.")
                ]
      hints' = [Hint $ "Use the variable '" <> unId var <> "' as an argument in another clause in the same rule."]
   in Err Nothing title markers hints'

wildcardInFactToReport :: FilePath -> Text -> SpanMap -> WildcardInFact -> Report Text
wildcardInFactToReport file' fileContent spanMap e@(WildcardInFact factNodeId' _ _pos) =
  let title = "Wildcard in top level fact"
      srcLocFact = getSourcePos file' fileContent spanMap factNodeId'
      srcLocArg = mainErrorPosition file' fileContent spanMap e
      markers = [ (srcLocArg, This "Wildcard found.")
                , (srcLocFact, Where "A top level fact only supports constants.\nVariables or wildcards are not allowed.")
                ]
      hints' = ["Replace the wildcard with a constant."]
   in Err Nothing title markers hints'

wildcardInRuleHeadToReport :: FilePath -> Text -> SpanMap -> WildcardInRuleHead -> Report Text
wildcardInRuleHeadToReport file' fileContent spanMap e@(WildcardInRuleHead ruleNodeId _ _pos) =
  let title = "Wildcard in 'head' of rule"
      srcLocRule = getSourcePos file' fileContent spanMap ruleNodeId
      srcLocArg = mainErrorPosition file' fileContent spanMap e
      markers = [ (srcLocArg, This "Wildcard found.")
                , (srcLocRule, Where "Only constants and variables are allowed in the head of a rule.\nWildcards are not allowed.")
                ]
      hints' = ["Replace the wildcard with a constant or a variable."]
   in Err Nothing title markers hints'

wildcardInAssignmentToReport :: FilePath -> Text -> SpanMap -> WildcardInAssignment -> Report Text
wildcardInAssignmentToReport file' fileContent spanMap e@(WildcardInAssignment assignNodeId _) =
  let title = "Found wildcard in equality constraint"
      srcLocWildcard = mainErrorPosition file' fileContent spanMap e
      srcLocAssign = getSourcePos file' fileContent spanMap assignNodeId
      markers = [ (srcLocWildcard, This "Wildcard found.")
                , (srcLocAssign, Where "Only constants and variables are allowed in an equality constraint.")
                ]
      hints' = ["This statement can be removed since it has no effect."]
   in Err Nothing title markers hints'

-- NOTE: pattern match is done this way to keep track of additional errors that need to be reported
{-# ANN semanticErrorsToReportsWithLocations ("HLint: ignore Use record patterns" :: String) #-}
semanticErrorsToReportsWithLocations :: FilePath -> Text -> SpanMap -> SemanticErrors -> [(Report Text, Location)]
semanticErrorsToReportsWithLocations file' fileContent spanMap e@(SemanticErrors _ _ _ _ _ _) =
  concat [ emptyModuleReports
         , ungroundedVarReports
         , variableInFactReports
         , wildcardInFactReports
         , wildcardInRuleHeadReports
         , wildcardInAssignmentReports
         ]
  where
    getReportsWithLocationsFor
      :: HasMainErrorPosition a
      => (SemanticErrors -> Container a)
      -> (FilePath -> Text -> SpanMap -> a -> Report Text)
      -> [(Report Text, Location)]
    getReportsWithLocationsFor f g =
      map (g file' fileContent spanMap &&& positionToLocation . mainErrorPosition file' fileContent spanMap) (f e)
    emptyModuleReports = getReportsWithLocationsFor emptyModules emptyModuleToReport
    ungroundedVarReports = getReportsWithLocationsFor ungroundedVars ungroundedVarToReport
    variableInFactReports = getReportsWithLocationsFor variablesInFacts variableInFactToReport
    wildcardInFactReports = getReportsWithLocationsFor wildcardsInFacts wildcardInFactToReport
    wildcardInRuleHeadReports = getReportsWithLocationsFor wildcardsInRuleHeads wildcardInRuleHeadToReport
    wildcardInAssignmentReports = getReportsWithLocationsFor wildcardsInAssignments wildcardInAssignmentToReport

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

positionToLocation :: Position -> Location
positionToLocation position =
  let locStart = uncurry Pos (bimap addOffset addOffset $ begin position)
      locEnd = uncurry Pos (bimap addOffset addOffset $ end position)
   in Location (file position) locStart locEnd
  where
    -- Diagnose is 1-based, Eclair is 0-based.
    addOffset :: Int -> Word32
    addOffset x = fromIntegral (x - 1)

class HasMainErrorPosition a where
  mainErrorPosition :: FilePath -> Text -> SpanMap -> a -> Position

instance HasMainErrorPosition TypeError where
  mainErrorPosition file' content spanMap = \case
    UnknownAtom nodeId _ ->
      getSourcePos file' content spanMap nodeId

    ArgCountMismatch _ _ (nodeId, _) ->
      getSourcePos file' content spanMap nodeId

    DuplicateTypeDeclaration _ decls ->
      let nodeId = fst $ head decls
       in getSourcePos file' content spanMap nodeId

    TypeMismatch nodeId _ _ _ ->
      getSourcePos file' content spanMap nodeId

    UnificationFailure _ _ ctx ->
      getSourcePos file' content spanMap $ getContextNodeId (last ctx)

    HoleFound nodeId _ _ _ ->
      getSourcePos file' content spanMap nodeId

instance HasMainErrorPosition EmptyModule where
  mainErrorPosition file' content spanMap (EmptyModule nodeId) =
    getSourcePos file' content spanMap nodeId

instance HasMainErrorPosition WildcardInAssignment where
  mainErrorPosition file' content spanMap (WildcardInAssignment _ nodeId) =
    getSourcePos file' content spanMap nodeId

instance HasMainErrorPosition UngroundedVar where
  mainErrorPosition file' content spanMap (UngroundedVar _ varNodeId _) =
    getSourcePos file' content spanMap varNodeId

instance HasMainErrorPosition VariableInFact where
  mainErrorPosition file' content spanMap (VariableInFact nodeId _) =
    getSourcePos file' content spanMap nodeId

instance HasMainErrorPosition WildcardInRuleHead where
  mainErrorPosition file' content spanMap (WildcardInRuleHead _ ruleArgId _) =
    getSourcePos file' content spanMap ruleArgId

instance HasMainErrorPosition WildcardInFact where
  mainErrorPosition file' content spanMap (WildcardInFact _ factArgId' _) =
    getSourcePos file' content spanMap factArgId'


-- Helper function to transform a Megaparsec error bundle into multiple reports
-- Extracted from the Diagnose library, and simplified for usage in Eclair.
errReportsWithLocationsFromBundle :: Text -> P.ParseErrorBundle Text Void -> [(Report Text, Location)]
errReportsWithLocationsFromBundle msg errBundle =
  toList (addLabelAndLocation <$> P.bundleErrors errBundle)
  where
    addLabelAndLocation e =
      let (_, pos) = P.reachOffset (P.errorOffset e) (P.bundlePosState errBundle)
          source = fromSourcePos (P.pstateSourcePos pos)
          msgs = lines $ toText (P.parseErrorTextPretty e)
          markers = case msgs of
            [m] ->
              [(source, This m)]
            [m1, m2] ->
              [(source, This m1), (source, Where m2)]
            _ ->
              [(source, This "<<Unknown error>>")]
          report = Err Nothing msg markers (errorHints e)
      in (report, positionToLocation source)

    fromSourcePos sourcePos =
      let both f = bimap f f
          begin' = both (fromIntegral . P.unPos) (P.sourceLine sourcePos, P.sourceColumn sourcePos)
          end' = second (+ 1) begin'
       in Position begin' end' (P.sourceName sourcePos)

    errorHints = \case
      P.TrivialError {} ->
        mempty
      P.FancyError _ errs ->
        S.toList errs >>= \case
          P.ErrorCustom e -> hints e
          _ -> mempty


data UseColor = UseColor
  deriving Eq

prettyError :: Maybe UseColor -> Diagnostic Text -> Doc AnsiStyle
prettyError useColor =
  applyStyle . prettyDiagnostic useUnicode tabSpaces
  where
    applyStyle =
      if useColor == Just UseColor
        then style
        else unAnnotate
    useUnicode = True
    tabSpaces = 2

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

instance HasHints Void msg where
  hints = const mempty
