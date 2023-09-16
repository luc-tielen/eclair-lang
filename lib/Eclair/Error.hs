{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclair.Error
  ( EclairError(..)
  , Issue(..)
  , Location(..)
  , Pos(..)
  , posToSourcePos
  , locationToSourceSpan
  , handleErrorsCLI
  , errorToIssues
  , renderIssueMessage
  ) where

import qualified Data.Map as M
import qualified Text.Megaparsec as P
import Data.List (partition)
import Eclair.AST.Analysis
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Common.Id
import Eclair.Common.Location
import Eclair.Souffle.IR
import Prettyprinter
import Prettyprinter.Render.Terminal
import Error.Diagnose hiding (stderr)

data EclairError
  = ParseErr FilePath ParsingError
  | TypeErr FilePath SpanMap [TypeError NodeId]
  | SemanticErr FilePath SpanMap (SemanticErrors NodeId)
  | ConversionErr FilePath SpanMap (ConversionError NodeId)

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
            pure $ "File not found: " <> pretty file' <> ".\n"
          ParsingError parseError -> do
            content <- decodeUtf8 <$> readFileBS file'
            let reports = map fst $ errReportsWithLocationsFromBundle "Failed to parse file" parseError
                diagnostic = foldl' addReport def reports
                diagnostic' = addFile diagnostic file' content
             in pure $ prettyError useColor diagnostic'

      TypeErr file' spanMap errs -> do
        content <- decodeUtf8 <$> readFileBS file'
        let errsWithPositions = getSourcePos file' content spanMap <<$>> errs
            reports = map typeErrorToReport errsWithPositions
            diagnostic = foldl' addReport def reports
            diagnostic' = addFile diagnostic file' (toString content)
         in pure $ prettyError useColor diagnostic'

      SemanticErr file' spanMap semanticErr -> do
        content <- decodeUtf8 <$> readFileBS file'
        let semanticErrsWithPositions = map (getSourcePos file' content spanMap) semanticErr
            reports = map fst $ semanticErrorsToReportsWithLocations semanticErrsWithPositions
            diagnostic = foldl' addReport def reports
            diagnostic' = addFile diagnostic file' (toString content)
         in pure $ prettyError useColor diagnostic'

      ConversionErr file' spanMap conversionErr -> do
        content <- decodeUtf8 <$> readFileBS file'
        let errWithPosition = map (getSourcePos file' content spanMap) conversionErr
            report = conversionErrorToReport errWithPosition
            diagnostic = addReport def report
            diagnostic' = addFile diagnostic file' (toString content)
        pure $ prettyError useColor diagnostic'

-- A single position in the code. 0-based!
data Pos
  = Pos
  { posLine :: {-# UNPACK #-} !Word32
  , posColumn :: {-# UNPACK #-} !Word32
  }

posToSourcePos :: Pos -> SourcePos
posToSourcePos (Pos l c) =
  SourcePos (fromIntegral l) (fromIntegral c)

-- Actual location in the code (a range).
-- Contains the file, start and end of the position.
-- TODO make this the leading location type instead of SourceSpan
data Location
  = Location
  { locationFile :: FilePath
  , locationStart :: {-# UNPACK #-} !Pos
  , locationEnd :: {-# UNPACK #-} !Pos
  }

locationToSourceSpan :: Location -> SourceSpan
locationToSourceSpan loc =
  SourceSpan (locationFile loc) posBegin posEnd
  where
    posBegin = posToSourcePos $ locationStart loc
    posEnd = posToSourcePos $ locationEnd loc

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
    let errsWithPositions = getSourcePos file' content spanMap <<$>> errs
        toLocation = positionToLocation . mainErrorPosition
    pure $ map (uncurry Issue . (typeErrorToReport &&& toLocation)) errsWithPositions

  SemanticErr file' spanMap semanticErrs -> do
    content <- readTextFile file'
    let semanticErrsWithPositions = map (getSourcePos file' content spanMap) semanticErrs
        reportsWithLocs = semanticErrorsToReportsWithLocations semanticErrsWithPositions
    pure $ map (uncurry Issue) reportsWithLocs

  ConversionErr file' spanMap conversionErr -> do
    content <- readTextFile file'
    let errWithPosition = map (getSourcePos file' content spanMap) conversionErr
        report = conversionErrorToReport errWithPosition
        loc = positionToLocation $ mainErrorPosition errWithPosition
    pure $ one $ Issue report loc

typeErrorToReport :: TypeError Position -> Report Text
typeErrorToReport e = case e of
  UnknownConstraint _ factName ->
    let title = "Missing type definition"
        markers = [(mainErrorPosition e, This $ "Could not find a type definition for '" <> unId factName <> "'.")]
        hints =
          [ Hint $ "Add a type definition for '" <> unId factName <> "'."
          , Hint $ "Add an extern definition for '" <> unId factName <> "'."
          ]
    in Err Nothing title markers hints

  UnknownFunction _ factName ->
    let title = "Missing type definition"
        markers = [(mainErrorPosition e, This $ "Could not find a type definition for '" <> unId factName <> "'.")]
        hints =
          [ Hint $ "Add an extern definition for '" <> unId factName <> "'." ]
    in Err Nothing title markers hints

  ArgCountMismatch factName (expectedSrcLoc, expectedCount) (actualSrcLoc, actualCount) ->
    let title = "Found an unexpected amount of arguments for '" <> unId factName <> "'"
        markers = [ (actualSrcLoc, This $ show actualCount <> pluralize actualCount " argument is" " arguments are" <> " provided here.")
                  , (expectedSrcLoc, Where $ "'" <> unId factName <> "' is defined with " <> show expectedCount <> " " <>
                    pluralize expectedCount "argument" "arguments" <> ".")
                  ]
        hints = [Hint $ "You can solve this by passing exactly " <> show expectedCount <> " "
          <> pluralize expectedCount "argument" "arguments" <> " to '" <> unId factName <> "'."]
    in Err Nothing title markers hints

  TypeMismatch _ actualTy expectedTy ctx ->
    Err Nothing title markers hints
      where
        title = "Type mismatch"
        lastMarker = (mainErrorPosition e, This $ show (length ctx + 1) <> ") Expected this to be of type " <> renderType expectedTy <> ","
                    <> " but it actually has type " <> renderType actualTy <> ".")
        markers = zipWith renderDeduction markerTypes (toList ctx) ++ [lastMarker]
        markerTypes = map (, Where) [1..]
        hints = []  -- Can we even give a meaningful error here? Look in type env? (if a var is used)

  UnificationFailure _ _ ctx ->
    Err Nothing title markers hints
      where
        title = "Type unification failure"
        markerTypes = markersForTypeError ctx
        markers = zipWith renderDeduction markerTypes (toList ctx)
        hints = []  -- What can we even give as a hint here? That it is a logical error?

  HoleFound _ ctx holeTy typeEnv ->
    Err Nothing title markers hints
      where
        title = "Found hole"
        markerTypes = map (, Where) [1..]
        deductions = zipWith renderDeduction markerTypes (toList ctx)
        markers = deductions <>
          [(mainErrorPosition e, This $ show (length deductions + 1) <> ") Found hole with type " <> renderType holeTy <> ".")]
        typeEntries =
          typeEnv
            & M.mapWithKey (\var ty -> (ty, renderBinding var ty))
            & toList
        (candidates, others) = partition (\(entryTy, _) -> entryTy == holeTy) typeEntries
        hints =
          map (Hint . (("Possible candidate: " <>) . snd)) candidates <>
          if null others
            then []
            else [Hint "Other variables include:"] <> map (Hint . snd) others
        renderBinding var ty =
          unId var <> " :: " <> renderType ty

  UnexpectedFunctionType _ defPos ->
    let title = "Invalid use of function"
        markers =
          [ (mainErrorPosition e, This "Expected a constraint here.")
          , (defPos, Where "Previously defined as a function here.")
          ]
        hints =
          [ Hint "Maybe you meant to declare this an external constraint instead?"
          , Hint "Remove the invalid function."
          ]
    in Err Nothing title markers hints

  UnexpectedConstraintType _ defPos ->
    let title = "Invalid use of constraint"
        markers =
          [ (mainErrorPosition e, This "Expected a function.")
          , (defPos, Where "Previously defined as a constraint here.")
          ]
        hints =
          [ Hint "Maybe you meant to declare this as a function instead?"
          , Hint "Remove the invalid constraint."
          ]
    in Err Nothing title markers hints
  where
    markersForTypeError ctx =
      zip [1..] $ replicate (length ctx - 1) Where ++ [This]

    renderDeduction :: (Int, Text -> Marker a) -> Context Position -> (Position, Marker a)
    renderDeduction (i, mkMarker) = \case
      WhileChecking srcLoc ->
        (srcLoc, mkMarker $ show i <> ") While checking the type of this..")
      WhileInferring srcLoc ->
        (srcLoc, mkMarker $ show i <> ") While inferring the type of this..")
      WhileUnifying srcLoc ->
        (srcLoc, mkMarker $ show i <> ") While unifying these types..")

conversionErrorToReport :: ConversionError Position -> Report Text
conversionErrorToReport e = case e of
  HoleNotSupported _ ->
    let title = "Unsupported feature in Souffle"
        markers = [(mainErrorPosition e, This "Souffle has no support for holes.")]
        hints = [Hint "Replace the hole with a variable or literal."]
    in Err Nothing title markers hints
  UnsupportedCase _ ->
    let title = "Unsupported feature in Souffle"
        markers = [(mainErrorPosition e, This "Eclair can't transpile extern definitions yet.")]
        hints = [Hint "Please open a github issue asking for this feature."]
     in Err Nothing title markers hints
  UnsupportedType _ ty ->
    let title = "Unsupported type in Souffle"
        markers = [(mainErrorPosition e, This $ "Souffle has no support for the " <> renderType ty <> " type.")]
        hints = []
     in Err Nothing title markers hints

ungroundedVarToReport :: UngroundedVar Position -> Report Text
ungroundedVarToReport e@(UngroundedVar srcLocRule _ var) =
  let title = "Ungrounded variable"
      srcLocVar = mainErrorPosition e
      markers = [ (srcLocVar, This $ "The variable '" <> unId var <> "' is ungrounded, meaning it is not directly bound as an argument to a relation.")
                , (srcLocRule, Where $ "This contains no clauses that refer to '" <> unId var <> "'.")
                ]
      hints = [Hint $ "Use the variable '" <> unId var <> "' as an argument in a relation."]
   in Err Nothing title markers hints

wildcardInFactToReport :: WildcardInFact Position -> Report Text
wildcardInFactToReport e@(WildcardInFact srcLocFact _ _pos) =
  let title = "Wildcard in top level fact"
      markers = [ (mainErrorPosition e, This "Wildcard found.")
                , (srcLocFact, Where "A top level fact only supports constants.\nVariables or wildcards are not allowed.")
                ]
      hints = [Hint "Replace the wildcard with a constant."]
   in Err Nothing title markers hints

wildcardInExternToReport :: WildcardInExtern Position -> Report Text
wildcardInExternToReport e@(WildcardInExtern atomLoc _ _) =
  Err Nothing title markers hints
  where
    title = "Wildcard in externally defined atom"
    markers = [ (mainErrorPosition e, This "Wildcard found.")
              , (atomLoc, Where "An external atom only supports constants or grounded variables.")
              ]
    hints = [Hint "Replace the wildcard with a constant or grounded variable."]

unconstrainedVarToReport :: UnconstrainedRuleVar Position -> Report Text
unconstrainedVarToReport e@(UnconstrainedRuleVar ruleLoc _ varName) =
  Err Nothing title markers hints
  where
    title = "Found unconstrained variable"
    markers = [ (mainErrorPosition e, This $ "The variable '" <> unId varName <> "' only occurs once.")
              , (ruleLoc, Where $ "This rule contains no other references to '" <> unId varName <> "'.")
              ]
    hints = [ Hint "Replace the variable with a wildcard ('_')."
            , Hint "Use the variable in another rule clause."
            ]

wildcardInRuleHeadToReport :: WildcardInRuleHead Position -> Report Text
wildcardInRuleHeadToReport e@(WildcardInRuleHead srcLocRule _ _pos) =
  let title = "Wildcard in 'head' of rule"
      markers = [ (mainErrorPosition e, This "Wildcard found.")
                , (srcLocRule, Where "Only constants and variables are allowed in the head of a rule.\nWildcards are not allowed.")
                ]
      hints = [Hint "Replace the wildcard with a constant or a variable."]
   in Err Nothing title markers hints

wildcardInConstraintToReport :: WildcardInConstraint Position -> Report Text
wildcardInConstraintToReport e@(WildcardInConstraint srcLocConstraint _) =
  let title = "Found wildcard in constraint"
      markers = [ (mainErrorPosition e, This "Wildcard found.")
                , (srcLocConstraint, Where "Only constants and variables are allowed in a constraint.")
                ]
      hints = [ Hint "This statement can be removed since it has no effect."
              , Hint "Replace the wildcard with a variable."
              ]
   in Err Nothing title markers hints

wildcardInBinOpToReport :: WildcardInBinOp Position -> Report Text
wildcardInBinOpToReport e@(WildcardInBinOp srcLocBinOp _) =
  let title = "Found wildcard in binary operation"
      markers = [ (mainErrorPosition e, This "Wildcard found.")
                , (srcLocBinOp, Where "Only constants and variables are allowed in a binary operation.")
                ]
      hints = [Hint "Replace the wildcard with a variable or literal."]
   in Err Nothing title markers hints

deadInternalRelationToReport :: DeadInternalRelation Position -> Report Text
deadInternalRelationToReport e@(DeadInternalRelation _ r) =
  let title = "Dead internal relation"
      markers = [(mainErrorPosition e, This $ "The internal rule '" <> unId r <> "' has no facts or rules defined and will never produce results.")]
      hints = [ Hint "This might indicate a logic error in your code."
              , Hint "Remove this rule if it is no longer needed."
              , Hint "Add 'input' to the declaration to indicate this rule is an input."
              ]
   in Err Nothing title markers hints

noOutputRelationsToReport :: NoOutputRelation Position -> Report Text
noOutputRelationsToReport e@(NoOutputRelation _) =
  let title = "No output relations found"
      markers = [(mainErrorPosition e, This "This module does not produce any results")]
      hints = [ Hint "Add an 'output' qualifier to one of the relations defined in this module." ]
  in Err Nothing title markers hints

conflictingDefinitionsToReport :: ConflictingDefinitionGroup Position -> Report Text
conflictingDefinitionsToReport e@(ConflictingDefinitionGroup name locs) =
  Err Nothing title (mainMarker:markers) hints
  where
    title = "Multiple definitions for '" <> unId name <> "'"
    mainMarker =
      (mainErrorPosition e, This $ "'" <> unId name <> "' is originally defined here.")
    markers = tail locs & toList & map (, Where $ "'" <> unId name <> "' is re-defined here.")
    hints = [Hint $ "You can solve this by removing the duplicate definitions for '" <> unId name <> "'."]

externUsedAsFactToReport :: ExternUsedAsFact Position -> Report Text
externUsedAsFactToReport e@(ExternUsedAsFact _ externLoc name) =
  Err Nothing title markers hints
  where
    title = "Extern definition used as top level fact"
    markers =
      [ (mainErrorPosition e, This $ "'" <> unId name <> "' is used as a fact here, which is not allowed for extern definitions.")
      , (externLoc, Where $ "'" <> unId name <> "' previously defined here as external.")
      ]
    hints = [ Hint $ "Convert '" <> unId name <> "' to a relation."
            , Hint "Remove the top level fact."
            ]

externUsedAsRuleToReport :: ExternUsedAsRule Position -> Report Text
externUsedAsRuleToReport e@(ExternUsedAsRule _ externLoc name) =
  Err Nothing title markers hints
  where
    title = "Extern definition used in rule head"
    markers =
      [ (mainErrorPosition e, This $ "'" <> unId name <> "' is used as a rule head here, which is not allowed for extern definitions.")
      , (externLoc, Where $ "'" <> unId name <> "' previously defined here as external.")
      ]
    hints = [ Hint $ "Convert '" <> unId name <> "' to a relation."
            , Hint "Remove the rule."
            ]

cyclicNegationToReport :: CyclicNegation Position -> Report Text
cyclicNegationToReport e =
  Err Nothing title markers hints
  where
    title = "Negation used in recursive set of rules"
    markers =
      [ (mainErrorPosition e, This "This negation is used in a set of rules that is recursive, which is not allowed.")
      ]
    hints = [ Hint "Restructure the program so the negation does not occur in the set of recursive rules."
            , Hint "Remove the negation entirely."
            ]


-- NOTE: pattern match is done this way to keep track of additional errors that need to be reported
{-# ANN semanticErrorsToReportsWithLocations ("HLint: ignore Use record patterns" :: String) #-}
semanticErrorsToReportsWithLocations :: SemanticErrors Position -> [(Report Text, Location)]
semanticErrorsToReportsWithLocations e@(SemanticErrors _ _ _ _ _ _ _ _ _ _ _ _ _) =
  concat [ ungroundedVarReports
         , wildcardInFactReports
         , wildcardInRuleHeadReports
         , wildcardInConstraintReports
         , wildcardInBinOpReports
         , wildcardInExternReports
         , unconstrainedVarReports
         , deadInternalRelationReports
         , noOutputReports
         , conflictingDefinitionReports
         , externUsedAsFactReports
         , externUsedAsRuleReports
         , cyclicNegationReports
         ]
  where
    getReportsWithLocationsFor
      :: HasMainErrorPosition a
      => (SemanticErrors Position -> Container a)
      -> (a -> Report Text)
      -> [(Report Text, Location)]
    getReportsWithLocationsFor f g =
      map (g &&& positionToLocation . mainErrorPosition) (f e)
    ungroundedVarReports = getReportsWithLocationsFor ungroundedVars ungroundedVarToReport
    wildcardInFactReports = getReportsWithLocationsFor wildcardsInFacts wildcardInFactToReport
    wildcardInRuleHeadReports = getReportsWithLocationsFor wildcardsInRuleHeads wildcardInRuleHeadToReport
    wildcardInConstraintReports = getReportsWithLocationsFor wildcardsInConstraints wildcardInConstraintToReport
    wildcardInBinOpReports = getReportsWithLocationsFor wildcardsInBinOps wildcardInBinOpToReport
    wildcardInExternReports = getReportsWithLocationsFor wildcardsInExternAtoms wildcardInExternToReport
    unconstrainedVarReports = getReportsWithLocationsFor unconstrainedVars unconstrainedVarToReport
    deadInternalRelationReports = getReportsWithLocationsFor deadInternalRelations deadInternalRelationToReport
    noOutputReports = getReportsWithLocationsFor noOutputRelations noOutputRelationsToReport
    conflictingDefinitionReports = getReportsWithLocationsFor conflictingDefinitions conflictingDefinitionsToReport
    externUsedAsFactReports = getReportsWithLocationsFor externsUsedAsFact externUsedAsFactToReport
    externUsedAsRuleReports = getReportsWithLocationsFor externsUsedAsRule externUsedAsRuleToReport
    cyclicNegationReports = getReportsWithLocationsFor cyclicNegations cyclicNegationToReport

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

renderType :: Type -> Text
renderType ty =
  let userFacingType = case ty of
        U32 -> "u32"
        Str -> "string"
        TUnknown x -> "t" <> show x
  in "'" <> userFacingType <> "'"

class HasMainErrorPosition a where
  mainErrorPosition :: a -> Position
instance HasMainErrorPosition (NoOutputRelation Position) where
  mainErrorPosition (NoOutputRelation pos) = startOfFile $ file pos
instance HasMainErrorPosition (DeadInternalRelation Position) where
  mainErrorPosition (DeadInternalRelation pos _) = pos
instance HasMainErrorPosition (WildcardInConstraint Position) where
  mainErrorPosition (WildcardInConstraint _ pos) = pos
instance HasMainErrorPosition (WildcardInBinOp Position) where
  mainErrorPosition (WildcardInBinOp _ pos) = pos
instance HasMainErrorPosition (WildcardInFact Position) where
  mainErrorPosition (WildcardInFact _ factArgPos _) = factArgPos
instance HasMainErrorPosition (WildcardInExtern Position) where
  mainErrorPosition (WildcardInExtern _ externArgPos _) = externArgPos
instance HasMainErrorPosition (UnconstrainedRuleVar Position) where
  mainErrorPosition (UnconstrainedRuleVar _ varPos _) = varPos
instance HasMainErrorPosition (UngroundedVar Position) where
  mainErrorPosition (UngroundedVar _ varPos _) = varPos
instance HasMainErrorPosition (WildcardInRuleHead Position) where
  mainErrorPosition (WildcardInRuleHead _ ruleArgPos _) = ruleArgPos
instance HasMainErrorPosition (ConflictingDefinitionGroup Position) where
  mainErrorPosition (ConflictingDefinitionGroup _ positions) = head positions
instance HasMainErrorPosition (ExternUsedAsFact Position) where
  mainErrorPosition (ExternUsedAsFact pos _ _) = pos
instance HasMainErrorPosition (ExternUsedAsRule Position) where
  mainErrorPosition (ExternUsedAsRule pos _ _) = pos
instance HasMainErrorPosition (CyclicNegation Position) where
  mainErrorPosition (CyclicNegation pos) = pos
instance HasMainErrorPosition (TypeError Position) where
  mainErrorPosition = \case
    UnknownConstraint pos _ -> pos
    UnknownFunction pos _ -> pos
    ArgCountMismatch _ _ (pos, _) -> pos
    TypeMismatch pos _ _ _ -> pos
    UnificationFailure _ _ ctx -> getContextLocation (last ctx)
    HoleFound pos _ _ _ -> pos
    UnexpectedFunctionType pos _ -> pos
    UnexpectedConstraintType pos _ -> pos
instance HasMainErrorPosition (ConversionError Position) where
  mainErrorPosition = \case
    HoleNotSupported pos -> pos
    UnsupportedType pos _ -> pos
    UnsupportedCase pos -> pos

-- Helper function to transform a Megaparsec error bundle into multiple reports
-- Extracted from the Diagnose library, and simplified for usage in Eclair.
errReportsWithLocationsFromBundle :: Text -> P.ParseErrorBundle Text CustomParseErr -> [(Report Text, Location)]
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
          report = Err Nothing msg markers mempty
      in (report, positionToLocation source)

    fromSourcePos sourcePos =
      let both f = bimap f f
          begin' = both (fromIntegral . P.unPos) (P.sourceLine sourcePos, P.sourceColumn sourcePos)
          end' = second (+ 1) begin'
       in Position begin' end' (P.sourceName sourcePos)


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
