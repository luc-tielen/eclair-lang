module Eclair.Parser
  ( parseFile
  , parseText
  , Parser
  , ParseError
  , CustomParseErr
  , ParsingError(..)
  ) where

import Data.Char
import Eclair.AST.IR
import Eclair.Common.Id
import Eclair.Common.Location
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Read as TR
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Internal as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as L

data CustomParseErr
  = TooManyInputOptions
  | TooManyOutputOptions
  deriving (Eq, Ord, Show)

instance P.ShowErrorComponent CustomParseErr where
  showErrorComponent = \case
    TooManyInputOptions ->
      "More than one option of type 'input' is not allowed."
    TooManyOutputOptions ->
      "More than one option of type 'output' is not allowed."

type ParseError = P.ParseErrorBundle Text CustomParseErr

type ParserState = (Word32, SpanMap)
type Parser = P.ParsecT CustomParseErr Text (State ParserState)

data ParsingError
  = FileNotFound FilePath
  | ParsingError ParseError
  deriving (Show)

parseFile
  :: (FilePath -> IO (Maybe Text))
  -> FilePath -> IO (AST, NodeId, SpanMap, Maybe ParsingError)
parseFile tryReadFile path = do
  mContents <- tryReadFile path
  case mContents of
    Nothing ->
      pure (emptyModule, NodeId 0, SpanMap path mempty, Just $ FileNotFound path)
    Just contents ->
      pure $ parseText path contents

parseText :: FilePath -> Text -> (AST, NodeId, SpanMap, Maybe ParsingError)
parseText path text =
  f $ runState (runParserT path text astParser) (0, SpanMap path mempty)
  where
    f ((mAst, mErr), s) =
      (fromMaybe emptyModule mAst, NodeId $ fst s, snd s, mErr)

emptyModule :: AST
emptyModule = Module (NodeId 0) mempty

-- Uses the internals of Megaparsec to try and return a parse result along
-- with possible parse errors.
runParserT :: FilePath -> Text -> Parser a -> State ParserState (Maybe a, Maybe ParsingError)
runParserT path text p = do
  let s = initialState path text
  (P.Reply s' _ result) <- P.runParsecT p s
  let toBundle es =
        P.ParseErrorBundle
          { P.bundleErrors = NE.sortWith P.errorOffset es,
            P.bundlePosState = P.statePosState s
          }
  pure $ case result of
    P.Error fatalError ->
      (Nothing, Just $ ParsingError $ toBundle $ fatalError :| P.stateParseErrors s')
    P.OK x ->
      let nonFatalErrs = viaNonEmpty toBundle (P.stateParseErrors s')
       in (Just x, ParsingError <$> nonFatalErrs)
  where
    initialState name s =
      P.State
        { P.stateInput = s,
          P.stateOffset = 0,
          P.statePosState =
            P.PosState
              { P.pstateInput = s,
                P.pstateOffset = 0,
                P.pstateSourcePos = P.initialPos name,
                P.pstateTabWidth = P.defaultTabWidth,
                P.pstateLinePrefix = ""
              },
          P.stateParseErrors = []
        }

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
    '@' -> do
      withNodeId $ \nodeId ->
        typedefParser nodeId <|> externParser nodeId
    _ -> factOrRuleParser

externParser :: NodeId -> Parser AST
externParser nodeId = do
  void $ lexeme $ P.chunk "@extern"
  name <- lexeme identifier
  tys <- lexeme $ betweenParens $ typeParser `P.sepBy1` lexeme comma
  mRetTy <- optional typeParser
  void $ P.char '.'
  pure $ ExternDefinition nodeId name tys mRetTy

typedefParser :: NodeId -> Parser AST
typedefParser nodeId = do
  void $ lexeme $ P.chunk "@def"
  name <- lexeme identifier
  tys <- lexeme $ betweenParens $ typeParser `P.sepBy1` lexeme comma
  attrs <- attributesParser
  void $ P.char '.'
  pure $ DeclareType nodeId name tys attrs
  where
    attributesParser = map (fromMaybe Internal) $ lexeme $ optional $ do
      options <- some attrParser
      let (inputs, outputs) = partitionEithers options
          inputLength = length inputs
          outputLength = length outputs
      when (inputLength > 1) $ do
        P.customFailure TooManyInputOptions
      when (outputLength > 1) $ do
        P.customFailure TooManyOutputOptions

      pure $ case (inputLength, outputLength) of
        (0, 1) -> Output
        (1, 0) -> Input
        _      -> InputOutput

    attrParser = lexeme $
      Left <$> P.chunk "input" <|> Right <$> P.chunk "output"

typeParser :: Parser Type
typeParser = lexeme $ u32 <|> str
  where
    u32 = U32 <$ P.chunk "u32"
    str = Str <$ P.chunk "string"

data FactOrRule = FactType | RuleType

factOrRuleParser :: Parser AST
factOrRuleParser = withNodeId $ \nodeId -> do
  name <- lexeme identifier
  args <- lexeme $ betweenParens $ exprParser `P.sepBy1` comma
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
  P.try (atomParser <* P.notFollowedBy opParser) <|> constraintParser
  where
    opParser =
      void constraintOpParser <|> void arithmeticOpParser
    arithmeticOpParser =
      P.choice $ concatMap (map $ P.char . snd) arithmeticOps

atomParser :: Parser AST
atomParser = do
  withNodeId $ \nodeId -> do
    name <- lexeme identifier
    args <- lexeme $ betweenParens $ exprParser `P.sepBy1` comma
    pure $ Atom nodeId name args

constraintParser :: Parser AST
constraintParser = withNodeId $ \nodeId -> do
  lhs <- lexeme exprParser
  op <- constraintOpParser
  rhs <- lexeme exprParser
  pure $ Constraint nodeId op lhs rhs

exprParser :: Parser AST
exprParser =
  lexeme $ withNodeId (L.makeExprParser termParser . precedenceTable)
  where
    precedenceTable nodeId =
      map (map (uncurry (binOp nodeId))) arithmeticOps
    binOp nodeId op c =
      L.InfixL (BinOp nodeId op <$ lexeme (P.char c))

    termParser =
      lexeme $ betweenParens exprParser <|> value
      where
        value = withNodeId $ \nodeId ->
          Hole nodeId <$ P.char '?' <|>
          P.try (varParser nodeId) <|>
          atomParser <|>
          Lit nodeId <$> literal

arithmeticOps :: [[(ArithmeticOp, Char)]]
arithmeticOps =
  [ [(Multiply, '*'), (Divide, '/')]
  , [(Plus, '+'), (Minus, '-')]
  ]

varParser :: NodeId -> Parser AST
varParser nodeId = do
  v <- lexeme $ Var nodeId <$> (identifier <|> wildcard)
  P.notFollowedBy $ P.char '('
  pure v

constraintOpParser :: Parser ConstraintOp
constraintOpParser = P.label "equality or comparison operator" $ lexeme $ do
  toOp Equals (P.char '=') <|>
    toOp LessOrEqual (P.string "<=") <|>
    toOp LessThan (P.char '<') <|>
    toOp GreaterOrEqual (P.string ">=") <|>
    toOp GreaterThan (P.char '>') <|>
    toOp NotEquals (P.string "!=")
  where toOp op p = op <$ lexeme p

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
  positiveNumber <|> zero
  where
    zero = 0 <$ P.char '0'

    positiveNumber = do
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
