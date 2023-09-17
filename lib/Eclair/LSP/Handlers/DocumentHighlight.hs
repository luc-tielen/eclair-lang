module Eclair.LSP.Handlers.DocumentHighlight
  ( documentHighlightHandler
  , DocHLResult(..)
  ) where

import Eclair
import Eclair.AST.IR
import Eclair.Common.Location
import Eclair.LSP.Monad

data DocHLResult
  = DocHLOk [SourceSpan]
  | DocHLError FilePath SourcePos Text
  deriving (Eq, Show)

documentHighlightHandler :: FilePath -> SourcePos -> LspM DocHLResult
documentHighlightHandler path srcPos = do
  let srcPos0 = toMachineSrcPos srcPos
  mFileContents <- lift $ vfsLookupFile path
  case mFileContents of
    Nothing ->
      pure $ DocHLError path srcPos "File not found in VFS!"
    Just fileContents ->
      case posToOffset srcPos0 fileContents of
        Left err ->
          pure $ DocHLError path srcPos err
        Right fileOffset -> do
          params <- getParams
          parseResult <- liftLSP $ runExceptT $ do
            (ast, spanMap) <- ExceptT (parse params path)
            let mNodeId = lookupNodeId spanMap fileOffset
            (ast, spanMap,) <$> liftEither (maybeToRight [] mNodeId)
          processParseResult fileContents parseResult
  where
    processParseResult fileContents = \case
      Left _errs ->
        pure $ DocHLError path srcPos "Failed to get highlight information!"
      Right (ast, spanMap, nodeId) -> do
        let refs = findReferences ast nodeId
            highlights = getHighlights path fileContents spanMap refs
        pure $ DocHLOk highlights

    getHighlights file fileContent spanMap =
      map (\refNodeId ->
        let span' = lookupSpan spanMap refNodeId
            sourceSpan = spanToSourceSpan file fileContent span'
        in sourceSpan)

-- TODO make this a build task
-- TODO implement for concepts besides variables
findReferences :: AST -> NodeId -> [NodeId]
findReferences ast nodeId =
  fst <$> zygo getVarId getRefs ast
  where
    getVarId = \case
      PWildcardF {} ->
        -- Wildcard matches with nothing.
        mempty
      VarF varNodeId var | nodeId == varNodeId ->
        First (Just var)
      astf ->
        fold astf

    getRefs = \case
      ModuleF _ decls ->
        foldMap snd $ filter (isJust . getFirst . fst) decls
      RuleF _ _ args clauses -> do
        let subtrees = args <> clauses
         in case getFirst $ foldMap fst subtrees of
              Nothing -> mempty
              Just var ->
                filter ((== var) . snd) $ foldMap snd subtrees
      VarF varNodeId var ->
        [(varNodeId, var)]
      astf ->
        foldMap snd astf
