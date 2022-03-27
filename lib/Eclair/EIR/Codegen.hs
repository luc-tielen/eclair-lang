module Eclair.EIR.Codegen
  ( CodegenM
  , runCodegen
  , ContainerInfo
  , LowerState(..)
  , CGState(..)
  , getLowerState
  , Relation
  , getFirstFieldOffset
  , idxFromConstraints
  , lookupRelationByIndex
  , lookupAlias
  , withUpdatedAlias
  , withEndLabel
  , withProjectState
  , withSearchState
  , block
  , declareType
  , fn
  , fnArg
  , call
  , fieldAccess
  , heapAllocProgram
  , freeProgram
  , stackAlloc
  , loop
  , jump
  , labelId
  , label
  , parallel
  , ret
  , var
  , assign
  , if'
  , not'
  , and'
  , equals
  , lit
  ) where

import Control.Monad.RWS.Strict
import Protolude hiding (Constraint, swap, from, to)
import Data.Maybe (fromJust)
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Eclair.RA.IndexSelection
import Eclair.TypeSystem
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.RA.IR as RA
import qualified Eclair.Runtime.Metadata as M
import qualified Eclair.Syntax as AST


type Id = AST.Id
type Alias = RA.Alias
type Relation = EIR.Relation
type EIR = EIR.EIR
type AliasMap = Map Alias EIR

type ContainerInfo = (Relation, Index, M.Metadata)

data LowerState
  = LowerState
  { typeEnv :: TypeInfo
  , idxMap :: IndexMap
  , idxSelector :: IndexSelector
  , containerInfos :: [ContainerInfo]
  , endLabel :: EIR.LabelId
  , aliasMap :: AliasMap
  }

data CGState
  = Normal LowerState
  | Search Alias EIR LowerState
  | Project Relation LowerState

type Count = Int
type IdMapping = Map Text Count

data Mapping
  = Mapping
  { labelMapping :: IdMapping
  , varMapping :: IdMapping
  }

instance Semigroup Mapping where
  (Mapping lbls1 vars1) <> (Mapping lbls2 vars2) =
    Mapping (combine lbls1 lbls2) (combine vars1 vars2)
    where combine = M.unionWith (+)

instance Monoid Mapping where
  mempty = Mapping mempty mempty

newtype CodegenM a
  = CodeGenM (RWS CGState () Mapping a)
  deriving ( Functor, Applicative, Monad
           , MonadReader CGState
           , MonadState Mapping
           )
  via (RWS CGState () Mapping)

runCodegen :: LowerState -> CodegenM EIR -> EIR
runCodegen ls (CodeGenM m) =
  fst $ evalRWS m (Normal ls) mempty

withSearchState :: Alias -> EIR -> CodegenM a -> CodegenM a
withSearchState alias value m = do
  ls <- getLowerState
  local (const $ Search alias value ls) m

withProjectState :: Relation -> CodegenM a -> CodegenM a
withProjectState relation m = do
  ls <- getLowerState
  local (const $ Project relation ls) m

getLowerState :: CodegenM LowerState
getLowerState = asks getLS
  where
    getLS = \case
      Normal ls -> ls
      Search _ _ ls -> ls
      Project _ ls -> ls

block :: [CodegenM EIR] -> CodegenM EIR
block ms = EIR.Block <$> sequence ms

-- TODO: declareProgram?
declareType :: [M.Metadata] -> CodegenM EIR
declareType metas = pure $ EIR.DeclareType metas

fn :: Text -> [EIR.EIRType] -> [CodegenM EIR] -> CodegenM EIR
fn name tys body = EIR.Function name tys <$> block body

fnArg :: Int -> CodegenM EIR
fnArg n = pure $ EIR.FunctionArg n

call :: EIR.EIRFunction -> [CodegenM EIR] -> CodegenM EIR
call fn args = EIR.Call fn <$> sequence args

fieldAccess :: CodegenM EIR -> Int -> CodegenM EIR
fieldAccess struct n = flip EIR.FieldAccess n <$> struct

heapAllocProgram :: CodegenM EIR
heapAllocProgram =
  pure EIR.HeapAllocateProgram

freeProgram :: CodegenM EIR -> CodegenM EIR
freeProgram ptr = EIR.FreeProgram <$> ptr

stackAlloc :: EIR.EIRType -> Relation -> CodegenM EIR
stackAlloc ty r = pure $ EIR.StackAllocate ty r

loop :: [CodegenM EIR] -> CodegenM EIR
loop ms = EIR.Loop <$> sequence ms

jump :: EIR.LabelId -> CodegenM EIR
jump lbl = pure $ EIR.Jump lbl

-- NOTE: labelId and label are split up, so label can be used in 2 ways:
-- 1) "endLabel" can also be passed into 'label'
-- 2) dynamic labels used for control flow can be generated with 'labelId' and passed to 'label'

labelId :: Text -> CodegenM EIR.LabelId
labelId name = do
  mapping <- gets labelMapping
  (labelId, updatedMapping) <- lookupId name mapping
  modify $ \s -> s { labelMapping = updatedMapping }
  pure . EIR.LabelId $ labelId

label :: EIR.LabelId -> CodegenM EIR
label = pure . EIR.Label

parallel :: [CodegenM EIR] -> CodegenM EIR
parallel ms = EIR.Par <$> sequence ms

ret :: CodegenM EIR -> CodegenM EIR
ret = map EIR.Return

-- NOTE: 2nd layer is for easy integration with other helper functions
-- e.g.:
--
-- do
--   v <- var "..."
--   sequence [ ... ]
var :: Text -> CodegenM (CodegenM EIR)
var name = do
  mapping <- gets varMapping
  (varId, updatedMapping) <- lookupId name mapping
  modify $ \s -> s { varMapping = updatedMapping }
  pure . pure . EIR.Var $ name

assign :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
assign var value = EIR.Assign <$> var <*> value

if' :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
if' cond body = EIR.If <$> cond <*> body

not' :: CodegenM EIR -> CodegenM EIR
not' bool = EIR.Not <$> bool

and' :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
and' lhs rhs = EIR.And <$> lhs <*> rhs

equals :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
equals lhs rhs = EIR.Equals <$> lhs <*> rhs

lit :: AST.Number -> CodegenM EIR
lit x = pure $ EIR.Lit x

lookupId :: Text -> IdMapping -> CodegenM (Text, IdMapping)
lookupId name mapping = do
  let (mValue, updatedMapping) = M.insertLookupWithKey update name defaultValue mapping
      value = fromMaybe defaultValue mValue
  pure (name <> "_" <> T.pack (show name), updatedMapping)
  where
    defaultValue = 0
    update _ newValue = const $ newValue + 1

getFieldOffset :: Relation -> Index -> CodegenM Int
getFieldOffset r idx = do
  cis <- containerInfos <$> getLowerState
  pure $ fromJust $ List.findIndex sameRelationAndIndex cis
  where
    sameRelationAndIndex (r', idx', _) =
      r == r' && idx == idx'

getFirstFieldOffset :: Relation -> CodegenM Int
getFirstFieldOffset r = do
  cis <- containerInfos <$> getLowerState
  pure $ fromJust $ List.findIndex sameRelation cis
  where
    sameRelation (r', _, _) = r == r'

lookupAlias :: Alias -> CodegenM EIR
lookupAlias a = ask >>= \case
  Normal ls -> lookupAlias' ls
  Search _ _ ls -> lookupAlias' ls
  Project _ ls -> lookupAlias' ls
  where
    lookupAlias' ls =
      pure $ fromJust $ M.lookup a (aliasMap ls)

withUpdatedAlias :: Alias -> CodegenM EIR -> CodegenM a -> CodegenM a
withUpdatedAlias a iter m = do
  -- TODO: emit?
  curr <- call EIR.IterCurrent [iter]
  state' <- ask <&> \case
    Normal ls -> Normal (updateAlias ls curr)
    Search a v ls -> Search a v (updateAlias ls curr)
    Project r ls -> Project r (updateAlias ls curr)
  local (const state') m
  where
    updateAlias ls curr =
      ls { aliasMap = M.insert a curr (aliasMap ls) }

withEndLabel :: EIR.LabelId -> CodegenM a -> CodegenM a
withEndLabel end m = do
  state <- ask
  local setLabel m
  where
    setLabel = \case
      Normal ls -> Normal (set ls)
      Search a v ls -> Search a v (set ls)
      Project r ls -> Project r (set ls)
    set ls = ls { endLabel = end }

idxFromConstraints :: Relation -> [(Relation, Column)] -> CodegenM (Index, [Column])
idxFromConstraints r constraints = do
    getIndexForSearch <- idxSelector <$> getLowerState
    let columns = mapMaybe (columnsForRelation r) constraints
        signature = SearchSignature $ S.fromList columns
        idx = getIndexForSearch r signature
    pure (idx, columns)

lookupRelationByIndex :: Relation -> Index -> CodegenM EIR
lookupRelationByIndex r idx = do
  field <- getFieldOffset r idx
  fieldAccess (fnArg 0) field
