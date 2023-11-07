{-# LANGUAGE CPP #-}

-- | Helper module to abstract away the differences between Eclair and Souffle Datalog.

#ifdef STAGE0

{-# LANGUAGE TypeApplications #-}

module Eclair.Datalog
  ( module Language.Souffle.Marshal
  , module Language.Souffle.Interpreted
  , module Language.Souffle.Analysis
  , DatalogM
  , FactOptions(..)
  , runDatalog
  ) where

import Prelude hiding (Handle)
import Language.Souffle.Marshal
import Language.Souffle.Interpreted hiding (FactOptions)
import Language.Souffle.Analysis

type DatalogM = SouffleM

newtype FactOptions a (dir :: Direction) (name :: Symbol)
  = FactOptions a

runDatalog :: Program prog => prog -> (Maybe (Handle prog) -> DatalogM a) -> IO a
runDatalog = runSouffle

instance Marshal fact => Marshal (FactOptions fact name dir) where
  push (FactOptions fact) = push fact
  {-# INLINABLE push #-}
  pop = FactOptions <$> pop
  {-# INLINABLE pop #-}

instance ( Marshal fact, KnownSymbol factName) => Fact (FactOptions fact dir factName) where
  type FactDirection (FactOptions _ dir _) = dir

  factName = const $ symbolVal (Proxy @factName)
  {-# INLINABLE factName #-}

#else

{-# LANGUAGE TemplateHaskell #-}

module Eclair.Datalog
  ( module Language.Eclair
  , module Language.Eclair.Marshal
  , module Language.Eclair.Analysis
  , DatalogM
  , ProgramOptions(..)
  , runDatalog
  , setNumThreads
  ) where

import Prelude hiding (Handle)
import Data.Kind
import Language.Eclair hiding (ProgramOptions)
import Language.Eclair.Analysis
import Language.Eclair.Marshal
import Language.Haskell.TH.Syntax hiding (Type)

type DatalogM = EclairM

-- Later this will be moved into eclair-haskell, when eclair supports running multiple analyses.
newtype ProgramOptions (a :: Type) (name :: Symbol) (facts :: [Type])
  = ProgramOptions a

instance Program (ProgramOptions a name facts) where
  type ProgramFacts (ProgramOptions _ _ facts) = facts

runDatalog :: Program prog => prog -> (Maybe (Handle prog) -> DatalogM a) -> IO a
runDatalog prog f = withEclair prog $ f . Just

setNumThreads :: Handle prog -> Word32 -> DatalogM ()
setNumThreads _ _ = pass

-- This line directly embeds the compiled Eclair code in the Haskell code
[] <$ qAddForeignFilePath RawObject "cbits/semantic_analysis.o"

#endif
