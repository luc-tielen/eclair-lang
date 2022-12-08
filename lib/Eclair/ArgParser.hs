module Eclair.ArgParser
  ( parseArgs
  , parser
  , Config(..)
  , CompileConfig(..)
  , EmitKind(..)
  , Target(..)
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
  , cpuTarget :: Maybe Target  -- Nothing = compile to host architecture
  } deriving (Eq, Show)

data Target
  = Wasm32
  deriving (Eq, Show)

data Config
  = Compile CompileConfig
  | LSP
  deriving (Eq, Show)


parseArgs :: [String] -> IO Config
parseArgs = handleParseResult . execParserPure parserPrefs parserInfo
  where
    desc = fullDesc <> progDesc "The Eclair Datalog compiler."
    parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
    parserInfo = info (parser <**> helper) desc

parser :: Parser Config
parser = hsubparser (longCompileCommand <> shortCompileCommand)
      <|> hsubparser lspCommand
  where
    longCompileCommand = command "compile" compileCommand
    shortCompileCommand = command "c" compileCommand
    compileCommand = info compileParser compileDesc
    compileDesc = fullDesc <> header "eclair compile" <> progDesc "Compiles Datalog files."
    lspCommand = command "lsp" $ info (pure LSP) lspDesc
    lspDesc = fullDesc <> header "eclair lsp" <> progDesc "Runs the Eclair LSP server."

compileParser :: Parser Config
compileParser = Compile <$> compileParser'
  where
    compileParser' =
      CompileConfig <$> argument str (metavar "FILE" <> help "The main Datalog file to compile.")
                    <*> emitKindParser
                    <*> optional targetParser

targetParser :: Parser Target
targetParser =
  option (maybeReader parseTarget) $ metavar "TARGET" <> long "target" <> short 't' <> help desc
  where
    desc = "Select the target CPU architecture. Default is to use the host architecture. Supported options: 'wasm32'."
    parseTarget = \case
      "wasm32" -> Just Wasm32
      _ -> Nothing

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
