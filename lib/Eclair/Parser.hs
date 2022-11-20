module Eclair.Parser
  ( parseFile
  , parseText
  , Parser
  , ParseError
  , ParseErr
  , ParsingError(..)
  , Span(..)
  , SpanMap
  , lookupSpan
  , SourcePos(..)
  , SourceSpan(..)
  , spanToSourceSpan
  ) where

import Data.Maybe (fromJust)
import Control.Monad.Fail
import Control.Monad.ST
import Data.Char
import Data.Void
import Eclair.AST.IR
import Eclair.Id
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Map as M
import qualified Data.Text.Read as TR
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import System.Directory.Extra (doesFileExist)

type ParseErr = Void
type ParseError = P.ParseErrorBundle Text ParseErr

data Span
  = Span
  { beginPos :: {-# UNPACK #-} !Int
  , endPos :: {-# UNPACK #-} !Int
  } deriving Show

data SpanMap =
  SpanMap
  { spanMapPath :: !FilePath
  , spanMapSpans :: !(Map Word32 Span)
  }
  deriving Show

insertSpan :: NodeId -> Span -> SpanMap -> SpanMap
insertSpan nodeId span (SpanMap path m) =
  SpanMap path (M.insert (unNodeId nodeId) span m)

-- NOTE: this assumes the node ID is generated by parsing the same file that resulted in the SpanMap.
lookupSpan :: SpanMap -> NodeId -> Span
lookupSpan (SpanMap _path m) nodeId =
  fromJust $ M.lookup (unNodeId nodeId) m

type ParserState = (Word32, SpanMap)
type Parser = P.ParsecT ParseErr Text (State ParserState)

data ParsingError
  = FileNotFound FilePath
  | ParsingError ParseError
  deriving (Show)

parseFile :: FilePath -> IO (Either ParsingError (AST, NodeId, SpanMap))
parseFile path = do
  fileExists <- doesFileExist path
  if fileExists
  then do
    contents <- readFileText path
    pure $ parseText path contents
  else
    pure $ Left $ FileNotFound path


parseText :: FilePath -> Text -> Either ParsingError (AST, NodeId, SpanMap)
parseText path text =
  g <$> f (runState (P.runParserT astParser path text) (0, SpanMap path mempty))
  where
    f (m, c) = case m of
      Left a -> Left (ParsingError a)
      Right b -> Right (b, c)
    g :: (AST, ParserState) -> (AST, NodeId, SpanMap)
    g (ast, ps) = (ast, NodeId $ fst ps, snd ps)

freshNodeId :: Parser NodeId
freshNodeId = do
  nodeId <- gets (NodeId . fst)
  modify $ first (+1)
  pure nodeId

withNodeId :: (NodeId -> Parser a) -> Parser a
withNodeId f = do
  nodeId <- freshNodeId
  begin <- P.getOffset
  parsed <- f nodeId
  end <- P.getOffset
  modify $ second (insertSpan nodeId (Span begin end))
  pure parsed

astParser :: Parser AST
astParser = withNodeId $ \nodeId -> do
  whitespace
  decls <-  withRecovery '.' declParser `P.endBy` whitespace
  P.eof
  pure $ Module nodeId $ catMaybes decls

declParser :: Parser AST
declParser = do
  c <- P.lookAhead P.anySingle
  case c of
    '@' -> typedefParser
    _ -> factOrRuleParser

typeParser :: Parser Type
typeParser = lexeme $ u32 <|> str
  where
    u32 = U32 <$ P.chunk "u32"
    str = Str <$ P.chunk "string"

typedefParser :: Parser AST
typedefParser = withNodeId $ \nodeId -> do
  void $ lexeme $ P.chunk "@def"
  name <- lexeme identifier
  tys <- betweenParens $ typeParser `P.sepBy1` lexeme comma
  void $ P.char '.'
  pure $ DeclareType nodeId name tys

data FactOrRule = FactType | RuleType

factOrRuleParser :: Parser AST
factOrRuleParser = withNodeId $ \nodeId -> do
  name <- lexeme identifier
  args <- lexeme $ betweenParens $ valueParser `P.sepBy1` comma
  declType <- lexeme (RuleType <$ P.chunk ":-") <|> (FactType <$ P.chunk ".")
  case declType of
    RuleType -> do
      body <- ruleClauseParser `P.sepBy1` comma <* period
      pure $ Rule nodeId name args body
    FactType -> pure $ Atom nodeId name args
  where
    period = P.char '.'

comma :: Parser Char
comma = lexeme $ P.char ','

ruleClauseParser :: Parser AST
ruleClauseParser = do
  atomParser <|> assignParser

atomParser :: Parser AST
atomParser = do
  P.notFollowedBy $ lexeme identifier *> P.char '='
  withNodeId $ \nodeId -> do
    name <- lexeme identifier
    args <- lexeme $ betweenParens $ valueParser `P.sepBy1` comma
    pure $ Atom nodeId name args

assignParser :: Parser AST
assignParser = withNodeId $ \nodeId -> do
  lhs <- lexeme valueParser
  _ <- lexeme $ P.char '='
  rhs <- lexeme valueParser
  pure $ Assign nodeId lhs rhs

valueParser :: Parser AST
valueParser = lexeme $ withNodeId $ \nodeId ->
  Var nodeId <$> (identifier <|> wildcard) <|>
  Lit nodeId <$> literal


-- Not sure if we want to support something like _abc?
wildcard :: Parser Id
wildcard =
  Id . one <$> P.char '_'

identifier :: Parser Id
identifier = Id <$> do
  firstLetter <- P.letterChar P.<?> "start of identifier"
  rest <- P.takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons firstLetter rest
  when (parsed `V.elem` reserved) $ do
    fail . toString $ "Reserved keyword: " <> parsed
  pure parsed
  where
    isIdentifierChar c = isAlphaNum c || c == '_'

-- List of reserved words, not allowed to be used in identifiers.
reserved :: V.Vector Text
reserved = V.fromList []

literal :: Parser Literal
literal = number <|> string

digitVector :: V.Vector Char
digitVector = V.fromList ['1'..'9']

number :: Parser Literal
number = LNumber <$> do
  firstDigit <- P.satisfy (`V.elem` digitVector) P.<?> "non-zero digit"
  digits <- P.takeWhileP Nothing isDigit
  P.notFollowedBy P.letterChar
  case TR.decimal $ T.cons firstDigit digits of
    Right (result, _) -> pure result
    Left err -> panic . toText $ "Error occurred during parsing of decimal number: " <> err

string :: Parser Literal
string = LString <$> do
  P.between ("\"" P.<?> "string literal") "\"" $
    toText <$> many (P.try escaped <|> normal)
  where
    escaped = do
      void $ P.char '\\'
      toEscapedChar <$> P.satisfy isEscapeChar
    isEscapeChar c =
      c `elem` ['"', '\\', 'n', 'r', 't', 'b', 'f', 'v', '0']
    toEscapedChar = \case
      '"' -> '\"'
      '\\' -> '\\'
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      'b' -> '\b'
      'f' -> '\f'
      'v' -> '\v'
      '0' -> '\0'
      _ -> panic "Unreachable code in string parser!"
    normal = P.anySingleBut '"'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = P.skipSome wsChar
  wsChar = void (P.satisfy $ \c -> c == ' ' || c == '\n') P.<?> "whitespace"
  commentParser = L.skipLineComment "//"
  blockCommentParser = L.skipBlockComment "/*" "*/"

betweenParens :: Parser a -> Parser a
betweenParens =
  P.between (lexeme $ P.char '(') (P.char ')') . lexeme

-- | Helper for parsers that can recover from errors.
--   In case of error, keeps parsing up to and including 'endChar'
withRecovery :: Char -> Parser a -> Parser (Maybe a)
withRecovery endChar p =
  P.withRecovery handleError $ map Just p
  where
    handleError err = do
      P.registerParseError err
      _ <- P.takeWhileP Nothing (/= endChar)
      _ <- P.char endChar
      pure Nothing

-- Helpers for producing error messages:

data SourcePos
  = SourcePos
  { sourcePosLine :: {-# UNPACK #-} !Int
  , sourcePosColumn :: {-# UNPACK #-} !Int
  }

data SourceSpan
  = SourceSpan
  { sourceSpanFile :: FilePath
  , sourceSpanBegin :: {-# UNPACK #-} !SourcePos
  , sourceSpanEnd :: {-# UNPACK #-} !SourcePos
  }

spanToSourceSpan :: FilePath -> Text -> Span -> SourceSpan
spanToSourceSpan path text span@(Span begin end) =
  either raiseError id parseResult
  where
    parseResult = P.runParser parser path text

    parser :: P.Parsec Void Text SourceSpan
    parser = do
      _ <- P.takeP Nothing begin
      beginPos <- P.getSourcePos
      _ <- P.takeP Nothing diff
      endPos <- P.getSourcePos
      let beginSourcePos = SourcePos (line beginPos) (column beginPos)
          endSourcePos = SourcePos (line endPos) (column endPos)
      pure $ SourceSpan path beginSourcePos endSourcePos
      where
        diff = end - begin
        line = P.unPos . P.sourceLine
        column = P.unPos . P.sourceColumn

    raiseError =
      const $ panic $ "Failed to get source location for file '" <> toText path <> "' and span " <> show span
