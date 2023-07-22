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
        let destinationPath = buildDir localBuildInfo </> "libsemantic_analysis" <.> "a"
        case buildOS of
          Windows -> die "Sorry, Eclair is not supported on Windows (yet)!"
          _ -> do
            let workPath = "cbits"
                eclairProgramPath = workPath </> "libsemantic_analysis.a"
            isAlreadyCompiled <- doesFileExist eclairProgramPath
            unless isAlreadyCompiled $ withCurrentDirectory workPath $ do
              void . system $ "clang -O2 -march=native -c -o semantic_analysis.o semantic_analysis.ll"
              -- TODO rm next line, looks like embedding .o file works better than linking with static lib
              -- void . system $ "ar rcs libsemantic_analysis.a semantic_analysis.o"
            copyFile eclairProgramPath destinationPath
    }
