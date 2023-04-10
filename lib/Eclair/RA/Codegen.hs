module Eclair.RA.Codegen
  ( CodegenM
  , runCodegen
  , LowerState(..)
  , mkLowerState
  , CGState(..)
  , CGInfo(..)
  , getLowerState
  , Relation
  , Alias
  , getFirstFieldOffset
  , getContainerInfoByOffset
  , idxFromConstraints
  , lookupRelationByIndex
  , lookupAlias
  , withUpdatedAlias
  , withEndLabel
  , withSearchState
  , block
  , declareProgram
  , fn
  , apiFn
  , fnArg
  , call
  , primOp
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
  , notEquals
  , lessThan
  , lessOrEqual
  , greaterThan
  , greaterOrEqual
  , mkExternOp
  , mkArithOp
  , plus
  , minus
  , multiply
  , divide
  , lit
  ) where

import Data.Maybe (fromJust)
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Set as S
import Eclair.RA.IndexSelection
import Eclair.TypeSystem
import Eclair.Common.Id
import Eclair.Common.Literal
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.RA.IR as RA
import qualified Eclair.LLVM.Metadata as Meta


type Alias = RA.Alias
type Relation = EIR.Relation
type EIR = EIR.EIR
type AliasMap = Map Alias EIR

-- Helper data type that pre-computes some data that is used a lot,
-- to speed up the compiler.
data CGInfo
  = CGInfo
  { relInfo :: [(Relation, Index)]
  , offsetsForRelationAndIndex :: Map (Relation, Index) Int
  , offsetsForRelation :: Map Relation Int  -- only tracks first one
  }

data LowerState
  = LowerState
  { typeEnv :: TypedefInfo
  , idxMap :: IndexMap
  , idxSelector :: IndexSelector
  , cgInfo :: CGInfo
  , endLabel :: EIR.LabelId
  , aliasMap :: AliasMap
  }

mkLowerState :: TypedefInfo -> IndexMap -> IndexSelector -> [(Relation, Index)] -> EIR.LabelId -> LowerState
mkLowerState typedefInfo indexMap getIndexForSearch relInfos end =
  LowerState typedefInfo indexMap getIndexForSearch codegenInfo end mempty
    where
      codegenInfo = CGInfo relInfos offsetsByRelationAndIndex offsetsByRelation
      offsetsByRelationAndIndex =
        M.fromDistinctAscList
          [ (ri, offset)
          | ri <- sortNub relInfos
          -- + 1 due to symbol table at position 0 in program struct
          , let offset = 1 + fromJust (List.elemIndex ri relInfos)
          ]
      offsetsByRelation =
        let rs = map fst relInfos
         in M.fromDistinctAscList
            [ (r, offset)
            | r <- sortNub rs
            -- + 1 due to symbol table at position 0 in program struct
            , let offset = 1 + fromJust (List.elemIndex r rs)
            ]

data CGState
  = Normal LowerState
  | Search Alias EIR LowerState

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

getLowerState :: CodegenM LowerState
getLowerState = asks getLS
  where
    getLS = \case
      Normal ls -> ls
      Search _ _ ls -> ls

block :: [CodegenM EIR] -> CodegenM EIR
block ms = do
  actions <- sequence ms
  pure $ EIR.Block $ flattenBlocks actions

flattenBlocks :: [EIR] -> [EIR]
flattenBlocks actions = flip concatMap actions $ \case
  EIR.Block stmts -> stmts
  stmt -> [stmt]

declareProgram :: [(Relation, Meta.Metadata)] -> CodegenM EIR
declareProgram metas = pure $ EIR.DeclareProgram metas

fn :: Text -> [EIR.Type] -> EIR.Type -> [CodegenM EIR] -> CodegenM EIR
fn name tys retTy body = EIR.Function EIR.Private name tys retTy <$> block body

apiFn :: Text -> [EIR.Type] -> EIR.Type -> [CodegenM EIR] -> CodegenM EIR
apiFn name tys retTy body = EIR.Function EIR.Public name tys retTy <$> block body

fnArg :: Int -> CodegenM EIR
fnArg n = pure $ EIR.FunctionArg n

call :: Relation -> Index -> EIR.Function -> [CodegenM EIR] -> CodegenM EIR
call r idx fn' =
  primOp (EIR.RelationOp r idx fn')

primOp :: EIR.Op -> [CodegenM EIR] -> CodegenM EIR
primOp op args =
  EIR.PrimOp op <$> sequence args

fieldAccess :: CodegenM EIR -> Int -> CodegenM EIR
fieldAccess struct n = flip EIR.FieldAccess n <$> struct

heapAllocProgram :: CodegenM EIR
heapAllocProgram =
  pure EIR.HeapAllocateProgram

freeProgram :: CodegenM EIR -> CodegenM EIR
freeProgram ptr = EIR.FreeProgram <$> ptr

stackAlloc :: Relation -> Index -> EIR.Type -> CodegenM EIR
stackAlloc r idx ty =
  pure $ EIR.StackAllocate (stripIdPrefixes r) idx ty

loop :: [CodegenM EIR] -> CodegenM EIR
loop ms = do
  actions <- sequence ms
  pure $ EIR.Loop $ flattenBlocks actions

jump :: EIR.LabelId -> CodegenM EIR
jump lbl = pure $ EIR.Jump lbl

-- NOTE: labelId and label are split up, so label can be used in 2 ways:
-- 1) "endLabel" can also be passed into 'label'
-- 2) dynamic labels used for control flow can be generated with 'labelId' and passed to 'label'

labelId :: Text -> CodegenM EIR.LabelId
labelId name = do
  mapping <- gets labelMapping
  (lblId, updatedMapping) <- lookupId name mapping
  modify $ \s -> s { labelMapping = updatedMapping }
  pure . EIR.LabelId $ lblId

label :: EIR.LabelId -> CodegenM EIR
label = pure . EIR.Label

parallel :: [CodegenM EIR] -> CodegenM EIR
parallel ms = do
  actions <- sequence ms
  pure $ EIR.Par $ flattenBlocks actions

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
  pure . pure . EIR.Var $ varId

assign :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
assign var' value = do
  v <- var'
  value >>= \case
    EIR.Block stmts ->
      let lastStmt = List.last stmts
          firstStmts = List.init stmts
       in block (map pure $ firstStmts ++ [EIR.Assign v lastStmt])
    val -> pure $ EIR.Assign v val

if' :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
if' cond body = do
  condition <- var "condition"
  block
    [ assign condition cond
    , EIR.If <$> condition <*> body
    ]

not' :: CodegenM EIR -> CodegenM EIR
not' bool' = EIR.Not <$> bool'

and' :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
and' lhs rhs = do
  lhsResult <- var "bool"
  rhsResult <- var "bool"
  block
    [ assign lhsResult lhs
    , assign rhsResult rhs
    , EIR.And <$> lhsResult <*> rhsResult
    ]

mkArithOp :: EIR.ArithmeticOp -> CodegenM EIR -> CodegenM EIR -> CodegenM EIR
mkArithOp op lhs rhs =
  let args = sequence [lhs, rhs]
   in EIR.PrimOp (EIR.ArithOp op) <$> args

mkExternOp :: Id -> [CodegenM EIR] -> CodegenM EIR
mkExternOp name args = do
  let program = EIR.FunctionArg 0
      symbolTable = EIR.FieldAccess program 0
  args' <- sequence args
  pure $ EIR.PrimOp (EIR.ExternOp name) $ symbolTable : args'

plus :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
plus = mkArithOp EIR.Plus

minus :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
minus = mkArithOp EIR.Minus

multiply :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
multiply = mkArithOp EIR.Multiply

divide :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
divide = mkArithOp EIR.Divide

mkConstrainOp :: EIR.LogicalOp -> CodegenM EIR -> CodegenM EIR -> CodegenM EIR
mkConstrainOp op lhs rhs =
  let args = sequence [lhs, rhs]
   in EIR.PrimOp (EIR.ComparisonOp op) <$> args

equals :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
equals = mkConstrainOp EIR.Equals

notEquals :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
notEquals = mkConstrainOp EIR.NotEquals

lessThan :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
lessThan = mkConstrainOp EIR.LessThan

lessOrEqual :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
lessOrEqual = mkConstrainOp EIR.LessOrEqual

greaterThan :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
greaterThan = mkConstrainOp EIR.GreaterThan

greaterOrEqual :: CodegenM EIR -> CodegenM EIR -> CodegenM EIR
greaterOrEqual = mkConstrainOp EIR.GreaterOrEqual

lit :: Word32 -> CodegenM EIR
lit x =
  pure $ EIR.Lit $ LNumber x

lookupId :: Text -> IdMapping -> CodegenM (Text, IdMapping)
lookupId name mapping = do
  let (mValue, updatedMapping) = M.insertLookupWithKey update name defaultValue mapping
      value = case mValue of
        Nothing -> name
        Just val -> name <> "_" <> show val
  pure (value, updatedMapping)
  where
    defaultValue = 1
    update _ _ prevValue = prevValue + 1

getFieldOffset :: Relation -> Index -> CodegenM Int
getFieldOffset r idx = do
  fromJust . M.lookup (r, idx) . offsetsForRelationAndIndex . cgInfo <$> getLowerState

getFirstFieldOffset :: Relation -> CodegenM Int
getFirstFieldOffset r = do
  fromJust . M.lookup r . offsetsForRelation . cgInfo <$> getLowerState

getContainerInfoByOffset :: Int -> CodegenM (Relation, Index)
getContainerInfoByOffset offset =
  -- - 1 due to symbol table at position 0 in program struct
  (List.!! (offset - 1)) . relInfo . cgInfo <$> getLowerState

lookupAlias :: Alias -> CodegenM EIR
lookupAlias a = ask >>= \case
  Normal ls -> lookupAlias' ls
  Search _ _ ls -> lookupAlias' ls
  where
    lookupAlias' ls =
      pure $ fromJust $ M.lookup a (aliasMap ls)

withUpdatedAlias :: Alias -> EIR -> CodegenM a -> CodegenM a
withUpdatedAlias a curr m = do
  state' <- ask <&> \case
    Normal ls -> Normal (updateAlias ls curr)
    Search a' v ls -> Search a' v (updateAlias ls curr)
  local (const state') m
  where
    updateAlias ls curr' =
      ls { aliasMap = M.insert a curr' (aliasMap ls) }

withEndLabel :: EIR.LabelId -> CodegenM a -> CodegenM a
withEndLabel end m = do
  local setLabel m
  where
    setLabel = \case
      Normal ls -> Normal (set ls)
      Search a v ls -> Search a v (set ls)
    set ls = ls { endLabel = end }

idxFromConstraints :: Relation -> Alias -> [(Relation, Column)] -> CodegenM Index
idxFromConstraints r a constraints = do
  lowerState <- getLowerState
  let (getIndexForSearch, indexMap) = (idxSelector &&& idxMap) lowerState
      r' = stripIdPrefixes r
  if null constraints
    then do
      -- NOTE: no constraints so we pick the longest index (total search)
      let mIndex = do
            indices <- M.lookup r' indexMap
            viaNonEmpty head $ sortOn (negate . length . unIndex) $ toList indices
      pure $ fromJust mIndex
    else do
      let columns = mapMaybe (columnsForRelation a) constraints
          signature = SearchSignature $ S.fromList columns
          idx = getIndexForSearch r signature
      pure idx

lookupRelationByIndex :: Relation -> Index -> CodegenM EIR
lookupRelationByIndex r idx = do
  field <- getFieldOffset r idx
  fieldAccess (fnArg 0) field
