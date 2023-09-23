module Eclair.LSP.Handlers.Hover
  ( hoverHandler
  , HoverResult(..)
  ) where

import Eclair
import Eclair.Error
import Eclair.TypeSystem hiding (typeCheck)
import Eclair.Common.Location
import Eclair.LSP.Monad
import qualified Data.Map as M

data HoverResult
  = HoverOk SourceSpan Type
  | HoverError FilePath SourcePos Text
  deriving (Eq, Show)

hoverHandler :: FilePath -> SourcePos -> LspM HoverResult
hoverHandler path srcPos = do
  mFileContents <- lift $ vfsLookupFile path
  case mFileContents of
    Nothing ->
      pure $ HoverError path srcPos "File not found in VFS!"
    Just fileContents ->
      case posToOffset srcPos fileContents of
        Left err ->
          pure $ HoverError path srcPos err
        Right fileOffset -> do
          processHoverOffset fileContents fileOffset
  where
    processHoverOffset fileContents fileOffset = do
      params <- getParams
      tcResult <- runExceptT $ do
        (ast, spanMap) <- ExceptT (liftLSP $ parse params path)
        (ast, spanMap,) <$> ExceptT (liftLSP $ typeCheck params path)
      case tcResult of
        Left errs ->
          processErrors fileContents errs
        Right (_, spanMap, typeInfo) ->
          processTypeInfo fileContents fileOffset spanMap typeInfo

    processTypeInfo fileContents fileOffset spanMap typeInfo = do
      let maybeResult = do
            nodeId <- lookupNodeId spanMap fileOffset
            ty <- M.lookup nodeId (resolvedTypes typeInfo)
            pure (nodeId, ty)
      case maybeResult of
        Nothing ->
          pure $ HoverError path srcPos "No type information for this position!"
        Just (nodeId, ty) -> do
          let span' = lookupSpan spanMap nodeId
              srcSpan = spanToSourceSpan path fileContents span'
          pure $ HoverOk srcSpan ty

    processErrors fileContents errs = do
      vfsVar <- lift getVfsVar
      vfs <- liftIO $ readMVar vfsVar
      issues <- traverse (liftLSP . errorToIssues (unsafeReadFromVFS vfs)) errs
      case findIssueAtPosition issues of
        Nothing ->
          pure $ HoverError path srcPos "File contains errors!"
        Just issue -> do
          let msg = renderIssueMessage path fileContents issue
          pure $ HoverError path srcPos msg

    findIssueAtPosition issues =
      flip find (concat issues) $ \i ->
        let loc = issueLocation i
            startPos' = posToSourcePos $ locationStart loc
            endPos' = posToSourcePos $ locationEnd loc
         in startPos' <= srcPos && srcPos <= endPos'
