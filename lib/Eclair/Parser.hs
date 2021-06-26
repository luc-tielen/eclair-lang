{-# LANGUAGE OverloadedLists #-}

module Eclair.Parser
  ( parseFile
  , printParseError
  , Parser
  , ParseError
  , ParseErr
  ) where

import Control.Monad.Fail
import Data.Char
import Data.Text (Text)
import Data.Vector as V
import Data.Void
import Eclair.Syntax
import Protolude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type ParseErr = Void
type ParseError = P.ParseErrorBundle Text ParseErr
type Parser = P.Parsec ParseErr Text

parseFile :: FilePath -> IO (Either ParseError AST)
parseFile path = do
  contents <- TIO.readFile path
  pure $ P.runParser astParser path contents

printParseError :: ParseError -> IO ()
printParseError err = putStrLn $ P.errorBundlePretty err

astParser :: Parser AST
astParser = do
  whitespace
  decls <- declParser `P.endBy` whitespace
  P.eof
  pure $ Module decls

data DeclType = AtomType | RuleType

declParser :: Parser AST
declParser = do
  name <- lexeme identifier
  args <- lexeme $ betweenParens $ valueParser `P.sepBy1` comma
  declType <- lexeme $ (RuleType <$ P.chunk ":-") <|> (AtomType <$ P.chunk ".")
  case declType of
    RuleType -> do
      body <- atomParser `P.sepBy1` comma <* period
      pure $ Rule name args body
    AtomType -> pure $ Atom name args
  where period = lexeme $ P.char '.'

comma :: Parser Char
comma = lexeme $ P.char ','

atomParser :: Parser AST
atomParser = do
  name <- lexeme identifier
  args <- lexeme $ betweenParens $ valueParser `P.sepBy1` comma
  pure $ Atom name args
valueParser :: Parser AST
valueParser =  Var <$> identifier
           <|> Lit <$> number

identifier :: Parser Id
identifier = Id <$> do
  first <- P.letterChar P.<?> "start of identifier"
  rest <- P.takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons first rest
  when (parsed `V.elem` reserved) $ do
    fail . T.unpack $ "Reserved keyword: " <> parsed
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
    Left err -> panic . T.pack $ "Error occurred during parsing of decimal number: " <> err

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

