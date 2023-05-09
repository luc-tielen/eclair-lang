{-# LANGUAGE TypeApplications, BangPatterns #-}
module Main where

import Criterion.Main
import Criterion.Types (Config(..))
import Control.DeepSeq
import GHC.Generics
import Data.Proxy
import qualified Language.Eclair as E hiding (Program, addFacts, run)
import qualified Language.Eclair.Internal as E
import qualified Data.Text.IO as TIO
import Graph
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Foldable

-- Copy pasted from eclair-haskell, so we can run it directly in IO (for use with Criterion)
addFacts
  :: forall f a. (Foldable f, E.Fact a, E.Sized (Rep a))
  => Ptr E.Program
  -> f a
  -> IO ()
addFacts prog facts = do
  let factCount = length facts
      bytesPerFact = E.toSize (Proxy @(Rep a))
      byteCount = factCount * bytesPerFact
  buffer <- mallocForeignPtrBytes byteCount
  withForeignPtr buffer $ \buf -> do
    let name = E.factName (Proxy @a)
        marshalState = E.MarshalState prog buf
    E.runMarshalM (traverse_ E.serialize facts) marshalState
    factType <- E.encodeString prog name
    E.addFacts prog factType buf (fromIntegral factCount)

loadGraph :: FilePath -> IO Graph
loadGraph path = do
  !txt <- TIO.readFile path
  parseGraph txt

setupEnv :: FilePath -> IO Env
setupEnv path = do
  !graph <- loadGraph path
  prog <- E.init
  withForeignPtr prog $ \ptr -> do
    addFacts ptr (vertices graph)
    addFacts ptr (edges graph)
  pure $ Env prog

-- Hack to get around NFData requirement
newtype Env = Env (ForeignPtr E.Program)
  deriving Generic
instance NFData Env where
  rnf Env{} = ()


reachableBenchmark :: String -> FilePath -> Benchmark
reachableBenchmark description path =
  bench description $ perRunEnv (setupEnv path) $ \(Env eclair) ->
    withForeignPtr eclair E.run

benchmarkConfig :: Config
benchmarkConfig =
  defaultConfig { jsonFile = Just "performance_results.json" }

main :: IO ()
main = defaultMainWith benchmarkConfig
  [ bgroup "reachable"
    [ reachableBenchmark "1k nodes, 10k edges" "./benchmarks/data/n1k_e10k.gr"
    , reachableBenchmark "1k nodes, 100k edges" "./benchmarks/data/n1k_e100k.gr"
    -- Following benchmark takes really long...
    -- , reachableBenchmark "10k nodes, 1M edges" "./benchmarks/data/n10k_e1m.gr"
    ]
  ]
