{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, TypeFamilies, LambdaCase #-}

module Main where

import qualified Language.Souffle.Compiled as Souffle
import GHC.Generics
import Control.Monad.IO.Class


data Path = Path

data Edge = Edge String String
  deriving (Generic, Souffle.Marshal)

data Reachable = Reachable String String
  deriving (Show, Generic, Souffle.Marshal)

instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Souffle.Fact Edge where
  type FactDirection Edge = 'Souffle.Input
  factName = const "edge"

instance Souffle.Fact Reachable where
  type FactDirection Reachable = 'Souffle.Output
  factName = const "reachable"


main :: IO ()
main = Souffle.runSouffle Path $ \case
  Nothing -> liftIO $ print "Failed to load Souffle program!"
  Just prog -> do
    Souffle.addFact prog $ Edge "a" "b"
    Souffle.addFacts prog $ [Edge "b" "c", Edge "a" "d", Edge "d" "e"]
    Souffle.run prog
    reachables <- Souffle.getFacts prog
    liftIO $ print (reachables :: [Reachable])

