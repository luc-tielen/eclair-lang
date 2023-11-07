{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Control.Monad
import Distribution.Simple
import Distribution.System (OS(..), buildOS)
import Distribution.Types.LocalBuildInfo
import System.Directory (copyFile, doesFileExist, withCurrentDirectory)
import System.FilePath ((<.>), (</>))
import System.Process (system)
import System.Exit (die)

[] <$ qAddDependentFile "cbits/semantic_analysis.ll"

-- TODO check for clang (+ version) also
main :: IO ()
main = defaultMainWithHooks $
  simpleUserHooks
    { postConf = \_args _configFlags _packageDescription localBuildInfo -> do
        case buildOS of
          Windows -> die "Sorry, Eclair is not supported on Windows (yet)!"
          _ -> do
            let workPath = "cbits"
                objPath = workPath </> "semantic_analysis" <.> "o"
            isAlreadyCompiled <- doesFileExist objPath
            unless isAlreadyCompiled $ withCurrentDirectory workPath $ do
              void . system $ "clang -O2 -march=native -c -o semantic_analysis.o semantic_analysis.ll"
    }
