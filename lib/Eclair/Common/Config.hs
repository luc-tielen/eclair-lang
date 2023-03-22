module Eclair.Common.Config
  ( EmitKind(..)
  , CompileConfig(..)
  , Target(..)
  , Config(..)
  ) where

data EmitKind
  = EmitTransformedAST
  | EmitRA
  | EmitTransformedRA
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
  , numCores :: Word -- Maximum number of cores Eclair is allowed to use
  } deriving (Eq, Show)

data Target
  = Wasm32
  deriving (Eq, Show)

data Config
  = Compile CompileConfig
  | LSP
  deriving (Eq, Show)
