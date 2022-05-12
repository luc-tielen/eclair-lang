{-# LANGUAGE OverloadedLists #-}

module Eclair.Parser
  ( parseFile
  , parseText
  , printParseError
  , Parser
  , ParseError
  , ParseErr
  , Span
  ) where

import Control.Monad.Fail
import Control.Monad.ST
import Data.Char
import Data.Void
import Eclair.AST.IR
import Eclair.Id
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.IntMap as M
import qualified Data.Text.Read as TR
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type ParseErr = Void
type ParseError = P.ParseErrorBundle Text ParseErr

data Span
  = Span
  { beginPos :: Int
  , endPos :: Int
  }

type ParserState = (Int, IntMap Span)
type Parser = P.ParsecT ParseErr Text (State ParserState)

parseFile :: FilePath -> IO (Either ParseError (AST, IntMap Span))
parseFile path = do
  contents <- readFileText path
  pure $ parseText path contents

parseText :: FilePath -> Text -> Either ParseError (AST, IntMap Span)
parseText path text =
  snd <<$>> f (runState (P.runParserT astParser path text) (0, mempty))
  where
    f :: (Either a b, c) -> Either a (b, c)
    f (m, c) = case m of
      Left a -> Left a
      Right b -> Right (b, c)

printParseError :: ParseError -> IO ()
printParseError err = putStrLn $ P.errorBundlePretty err

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
  -- TODO: figure out if we can update a vector directly here without issues in ordering
  modify $ second (M.insert (unNodeId nodeId) (Span begin end))
  pure parsed

astParser :: Parser AST
astParser = withNodeId $ \nodeId -> do
  whitespace
  decls <- declParser `P.endBy` whitespace
  P.eof
  pure $ Module nodeId decls

declParser :: Parser AST
declParser = do
  c <- P.lookAhead P.anySingle
  case c of
    '@' -> typedefParser
    _ -> factOrRuleParser

typeParser :: Parser Type
typeParser = lexeme $
  U32 <$ P.chunk "u32"

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
  declType <- lexeme $ (RuleType <$ P.chunk ":-") <|> (FactType <$ P.chunk ".")
  case declType of
    RuleType -> do
      body <- atomParser `P.sepBy1` comma <* period
      pure $ Rule nodeId name args body
    FactType -> pure $ Atom nodeId name args
  where period = lexeme $ P.char '.'

comma :: Parser Char
comma = lexeme $ P.char ','

atomParser :: Parser AST
atomParser = withNodeId $ \nodeId -> do
  name <- lexeme identifier
  args <- lexeme $ betweenParens $ valueParser `P.sepBy1` comma
  pure $ Atom nodeId name args

valueParser :: Parser AST
valueParser = withNodeId $ \nodeId ->
  Var nodeId <$> identifier <|> Lit nodeId <$> number

identifier :: Parser Id
identifier = Id <$> do
  firstLetter <- P.letterChar P.<?> "start of identifier"
  rest <- P.takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons firstLetter rest
  when (parsed `V.elem` reserved) $ do
    fail . toString $ "Reserved keyword: " <> parsed
  pure parsed
  where isIdentifierChar c = isAlphaNum c || c == '_'
        reserved = []

number :: Parser Number
number = do
  firstDigit <- P.satisfy (`V.elem` ['1'..'9']) P.<?> "non-zero digit"
  digits <- P.takeWhileP Nothing isDigit
  P.notFollowedBy P.letterChar
  case TR.decimal $ T.cons firstDigit digits of
    Right (result, _) -> pure result
    Left err -> panic . toText $ "Error occurred during parsing of decimal number: " <> err

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

