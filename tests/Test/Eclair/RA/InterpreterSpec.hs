module Test.Eclair.RA.InterpreterSpec
  ( module Test.Eclair.RA.InterpreterSpec
  ) where


import qualified Eclair as E
import Eclair.Id
import Eclair.AST.IR
import Eclair.RA.IR
import Protolude hiding ((<.>))
import Test.Hspec
import qualified Data.Map as M
import System.FilePath

type Record = [Number]

interpret :: FilePath -> IO (M.Map Relation [Record])
interpret path = do
  let file = "tests/fixtures/interpreter" </> path <.> "dl"
  E.run file

spec :: Spec
spec = describe "RA interpreter" $ parallel $ do
  it "can interpret simple facts" $ do
    result <- interpret "single_fact"
    result `shouldBe` M.fromList
      [ (Id "edge", [[1,2], [2,3]])
      , (Id "another", [[1,2,3]])
      ]

  it "can interpret non recursive rules" $ do
    result <- interpret "single_nonrecursive_rule"
    result `shouldBe`
      M.fromList
      [ (Id "a", [[1,2]])
      , (Id "b", [[1,2]])
      , (Id "c", [[3], [4]])
      , (Id "d", [[4], [3]])
      , (Id "e", [[4], [5]])
      , (Id "f", [[4]])
      ]

  it "can interpret rules with multiple clauses" $ do
    result <- interpret "multiple_clauses_in_rule"
    result `shouldBe`
      M.fromList
      [ (Id "fact", [[1], [2], [3]])
      , (Id "product", [ [3,3], [3,2], [3,1]
                       , [2,3], [2,2], [2,1]
                       , [1,3], [1,2], [1,1]])
      ]

  it "can interpret rules with multiple clauses of same name" $ do
    result <- interpret "multiple_clauses_same_name"
    result `shouldBe`
      M.fromList
      [ (Id "link", [[1,2], [2,3], [3, 4]])
      , (Id "chain", [[2,3,4], [1,2,3]])
      ]

  it "can interpret a single recursive rule" $ do
    result <- interpret "single_recursive_rule"
    result `shouldBe` M.fromList
      [ (Id "edge", [[1,2], [2,3], [3,4], [5,6]])
      , (Id "path", [[1,4], [2,4], [1,3], [5,6], [3,4], [2,3], [1,2]])
      ]

  it "can interpret mutually recursive rules" $ do
    result <- interpret "mutually_recursive_rule"
    result `shouldBe` M.fromList
      [ (Id "a", [[1], [2]])
      , (Id "b", [[2], [1]])
      , (Id "c", [[1], [2]])
      , (Id "d", [[1], [2], [3]])
      ]
