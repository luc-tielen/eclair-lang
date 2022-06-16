module Eclair.ArgParser
  ( parseArgs
  , parser
  , Config(..)
  , CompileConfig(..)
  , EmitKind(..)
  ) where

import Options.Applicative
import qualified Data.List.Extra as L


data EmitKind
  = EmitSimplifiedAST
  | EmitRA
  | EmitEIR
  | EmitLLVM
  -- TODO: object file, WASM, ...
  deriving (Eq, Show)

-- TODO: optimization levels (-Ox), include dirs (-I), logging level (-q, -v), timing, ...
data CompileConfig
  = CompileConfig
  { mainFile :: FilePath
  , emitKind :: EmitKind
  } deriving (Eq, Show)

newtype Config
  = Compile CompileConfig
  -- TODO add LSP
  deriving (Eq, Show)


parseArgs :: [String] -> IO Config
parseArgs = handleParseResult . execParserPure parserPrefs parserInfo
  where
    desc = fullDesc <> progDesc "The Eclair Datalog compiler."
    parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    parserInfo = info (parser <**> helper) desc

parser :: Parser Config
parser = hsubparser $ longCompileCommand <> shortCompileCommand
  where
    longCompileCommand = command "compile" compileCommand
    shortCompileCommand = command "c" compileCommand
    compileCommand = info compileParser compileDesc
    compileDesc = fullDesc <> header "eclair compile" <> progDesc "Compiles Datalog files."

compileParser :: Parser Config
compileParser = Compile <$> compileParser'
  where
    compileParser' =
      CompileConfig <$> argument str (metavar "FILE" <> help "The main Datalog file to compile.")
                    <*> emitKindParser

emitKindParser :: Parser EmitKind
emitKindParser =
  option (maybeReader readEmitKind) (long "emit" <> value EmitLLVM <> help desc)
  where
    readEmitKind opt = case L.lower opt of
      "ast-simplified" -> Just EmitSimplifiedAST
      "ra" -> Just EmitRA
      "eir" -> Just EmitEIR
      "llvm" -> Just EmitLLVM
      _ -> Nothing
    desc = "Emit a specific IR. Defaults to emitting LLVM IR. Supported options: 'ast-simplified', 'ra', 'eir', 'llvm'."
