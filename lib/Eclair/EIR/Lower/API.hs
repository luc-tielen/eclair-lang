{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.EIR.Lower.API
  ( CodegenInOutT
  , mkInOutState
  , codegenAPI
  , apiFunction
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import Data.Traversable (for)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.SymbolTable as SymbolTable
import Eclair.LLVM.Codegen as LLVM
import Eclair.LLVM.Metadata
import Eclair.EIR.Lower.Codegen
import Eclair.AST.IR (UsageMode(..))
import Eclair.RA.IndexSelection
import Eclair.Common.Id


type Relation = EIR.Relation

-- Helper data type that pre-computes most of the important data.
data InOutState
  = InOutState
  { relationMapping :: Map Relation Word32
  , relationNumColumns :: Map Relation Int
  , relations :: Set Relation
  , indicesByRelation :: Map Relation [Index]
  , offsetsByRelationAndIndex :: Map (Relation, Index) Int
  , inOutLowerState :: LowerState
  }

type CodegenInOutT = ReaderT InOutState

codegenAPI :: Map Id Word32 -> Map Relation UsageMode -> [(Relation, Metadata)] -> LowerState -> ModuleBuilderT IO ()
codegenAPI relMapping usageMapping metas lowerState = do
  usingReaderT (mkInOutState relMapping metas lowerState) $ do
    addFactsFn <- generateAddFactsFn usageMapping
    _ <- generateAddFact addFactsFn
    _ <- generateGetFactsFn usageMapping
    _ <- generateFreeBufferFn
    _ <- generateFactCountFn usageMapping
    _ <- generateEncodeStringFn
    _ <- generateDecodeStringFn
    pass

generateAddFact :: MonadFix m => Operand -> CodegenInOutT (ModuleBuilderT m) Operand
generateAddFact addFactsFn = do
  lowerState <- asks inOutLowerState
  let args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i32, ParameterName "fact_type")
             , (ptr i32, ParameterName "memory")
             ]
      returnType = void

  apiFunction "eclair_add_fact" args returnType $ \[program, factType, memory] -> do
    _ <- call addFactsFn [program, factType, memory, int32 1]
    retVoid

generateAddFactsFn :: MonadFix m => Map Id UsageMode -> CodegenInOutT (ModuleBuilderT m) Operand
generateAddFactsFn usageMapping = do
  inOutState <- ask
  let lowerState = inOutLowerState inOutState
      rels = S.filter (isInputRelation usageMapping) $ relations inOutState
      args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i32, ParameterName "fact_type")
             , (ptr i32, ParameterName "memory")
             , (i32, ParameterName "fact_count")
             ]
      returnType = void
  apiFunction "eclair_add_facts" args returnType $ \[program, factType, memory, factCount] -> mdo
    switchOnFactType rels (relationMapping inOutState) retVoid factType $ \r -> do
      indexes <- indicesForRelation r
      for_ indexes $ \idx -> do
        numCols <- fromIntegral <$> numColsForRelation r
        treeOffset <- int32 . toInteger <$> offsetForRelationAndIndex r idx
        relationPtr <- gep program [int32 0, treeOffset]
        -- TODO: don't re-calculate this type, do this based on value datatype created in each runtime data structure
        let arrayPtr = ptrcast (ArrayType numCols i32) memory

        loopFor (int32 0) (`ult` factCount) (add (int32 1)) $ \i -> do
          valuePtr <- gep arrayPtr [i]
          fn <- toCodegenInOut lowerState $ lookupFunction r idx EIR.Insert
          call fn [relationPtr, valuePtr]

      br end -- early return

    end <- blockNamed "end"
    retVoid

generateGetFactsFn :: MonadFix m => Map Relation UsageMode -> CodegenInOutT (ModuleBuilderT m) Operand
generateGetFactsFn usageMapping = do
  inOutState <- ask
  let lowerState = inOutLowerState inOutState
      rels = S.filter (isOutputRelation usageMapping) $ relations inOutState
      args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i32, ParameterName "fact_type")
             ]
      returnType = ptr i32
      mallocFn = extMalloc $ externals lowerState
  apiFunction "eclair_get_facts" args returnType $ \[program, factType] -> do
    switchOnFactType rels (relationMapping inOutState) (ret $ nullPtr i32) factType $ \r -> do
      indexes <- indicesForRelation r
      let idx = fromJust $ findLongestIndex indexes
          doCall op args' = do
            fn <- toCodegenInOut lowerState $ lookupFunction r idx op
            call fn args'
      numCols <- numColsForRelation r
      let valueSize = 4 * numCols  -- TODO: should use LLVM "valueSize" instead of re-calculating here
      treeOffset <- int32 . toInteger <$> offsetForRelationAndIndex r idx
      relationPtr <- gep program [int32 0, treeOffset]
      relationSize <- doCall EIR.Size [relationPtr] >>= (`trunc` i32)
      memorySize <- mul relationSize (int32 $ toInteger valueSize)
      memory <- call mallocFn [memorySize]
      let arrayPtr = ptrcast (ArrayType (fromIntegral numCols) i32) memory

      iPtr <- alloca i32 (Just (int32 1)) 0
      store iPtr 0 (int32 0)
      let iterTy = evalState (toLLVMType r idx EIR.Iter) lowerState
      currIter <- alloca iterTy (Just (int32 1)) 0
      endIter <- alloca iterTy (Just (int32 1)) 0
      _ <- doCall EIR.IterBegin [relationPtr, currIter]
      _ <- doCall EIR.IterEnd [relationPtr, endIter]
      let loopCondition = do
            isEqual <- doCall EIR.IterIsEqual [currIter, endIter]
            not' isEqual
      loopWhile loopCondition $ do
        i <- load iPtr 0
        valuePtr <- gep arrayPtr [i]
        currentVal <- doCall EIR.IterCurrent [currIter]
        copy (mkPath []) currentVal valuePtr
        i' <- add i (int32 1)
        store iPtr 0 i'
        doCall EIR.IterNext [currIter]

      ret $ ptrcast i32 memory

generateFreeBufferFn :: Monad m => CodegenInOutT (ModuleBuilderT m) Operand
generateFreeBufferFn = do
  lowerState <- asks inOutLowerState
  let freeFn = extFree $ externals lowerState
      args = [(ptr i32, ParameterName "buffer")]
      returnType = void
  apiFunction "eclair_free_buffer" args returnType $ \[buf] -> mdo
    let memory = ptrcast i8 buf
    _ <- call freeFn [memory]
    retVoid

generateFactCountFn :: MonadFix m => Map Id UsageMode -> CodegenInOutT (ModuleBuilderT m) Operand
generateFactCountFn usageMapping = do
  inOutState <- ask
  let lowerState = inOutLowerState inOutState
      rels = S.filter (isOutputRelation usageMapping) $ relations inOutState
      args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i32, ParameterName "fact_type")
             ]
      returnType = i32
  apiFunction "eclair_fact_count" args returnType $ \[program, factType] -> do
    switchOnFactType rels (relationMapping inOutState) (ret $ int32 0) factType $ \r -> do
      indexes <- indicesForRelation r
      let idx = fromJust $ findLongestIndex indexes
          doCall op args' = do
            fn <- toCodegenInOut lowerState $ lookupFunction r idx op
            call fn args'
      treeOffset <- int32 . toInteger <$> offsetForRelationAndIndex r idx
      relationPtr <- gep program [int32 0, treeOffset]
      relationSize <- doCall EIR.Size [relationPtr]
      ret =<< trunc relationSize i32


-- NOTE: string does not need to be 0-terminated, length field is used to determine length (in bytes).
-- Eclair makes an internal copy of the string, for simpler memory management.
generateEncodeStringFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateEncodeStringFn = do
  lowerState <- asks inOutLowerState
  let args = [ (ptr (programType lowerState), "eclair_program")
             , (i32, "string_length")
             , (ptr i8, "string_data")
             ]
      (symbolTable, symbol) = (symbolTableFns &&& symbolFns) lowerState
      exts = externals lowerState

  apiFunction "eclair_encode_string" args i32 $ \[program, len, stringData] -> do
    stringDataCopy <- call (extMalloc exts) [len]
    lenBytes <- zext len i64
    _ <- call (extMemcpy exts) [stringDataCopy, stringData, lenBytes, bit 0]

    symbolPtr <- alloca (Symbol.tySymbol symbol) (Just (int32 1)) 0
    _ <- call (Symbol.symbolInit symbol) [symbolPtr, len, stringDataCopy]

    symbolTablePtr <- getSymbolTablePtr program
    index <- call (SymbolTable.symbolTableLookupIndex symbolTable) [symbolTablePtr, symbolPtr]
    alreadyContainsSymbol <- index `ne` int32 0xFFFFFFFF
    if' alreadyContainsSymbol $ do
      -- Since the string was not added to the table, the memory pointed to by
      -- the symbol is not managed by the symbol table, so we need to manually free the data.
      _ <- call (extFree exts) [stringDataCopy]
      ret index

    -- No free needed here, automatically called when symbol table is cleaned up.
    ret =<< call (SymbolTable.symbolTableFindOrInsert symbolTable) [symbolTablePtr, symbolPtr]

-- NOTE: do not free the returned string/byte array,
-- this happens automatically when eclair_destroy is called
generateDecodeStringFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateDecodeStringFn = do
  lowerState <- asks inOutLowerState
  let args = [ (ptr (programType lowerState), "eclair_program")
             , (i32, "string_index")
             ]
      symbolTable = symbolTableFns lowerState

  apiFunction "eclair_decode_string" args (ptr i8) $ \[program, idx] -> do
    symbolTablePtr <- getSymbolTablePtr program
    containsIndex <- call (SymbolTable.symbolTableContainsIndex symbolTable) [symbolTablePtr, idx]
    if' containsIndex $ do
      symbolPtr <- call (SymbolTable.symbolTableLookupSymbol symbolTable) [symbolTablePtr, idx]
      ret $ ptrcast i8 symbolPtr

    ret $ nullPtr i8

toCodegenInOut :: Monad m => LowerState -> CodegenT m Operand -> IRBuilderT (CodegenInOutT (ModuleBuilderT m)) Operand
toCodegenInOut lowerState m =
  hoist lift $ runCodegenM m lowerState

mkInOutState :: Map Relation Word32 -> [(Relation, Metadata)] -> LowerState -> InOutState
mkInOutState relMapping metas ls =
  InOutState relMapping relNumCols rels indicesByRel offsetsByRelAndIndex ls
    where
      rs = map fst metas
      -- NOTE: disregards all "special" relations, since they should not be visible to the end user!
      rels = S.fromList $ filter (not . startsWithIdPrefix) rs
      relNumCols =
        M.fromList [ (r, numCols)
                   | r <- rs
                   , let numCols = getNumColumns (snd $ fromJust $ L.find ((== r) . fst) metas)
                   ]
      relInfos = M.keys $ fnsMap ls
      indicesByRel =
        M.fromAscListWith (<>) $ map (second one) $ sortWith fst relInfos
      offsetsByRelAndIndex =
        M.fromDistinctAscList
          [ (ri, offset)
          | ri <- sortNub relInfos
          -- + 1 due to symbol table at position 0 in program struct
          , let offset = 1 + fromJust (L.elemIndex ri relInfos)
          ]

indicesForRelation :: MonadReader InOutState m => Relation -> m [Index]
indicesForRelation r = do
  inOutState <- ask
  pure . fromJust . M.lookup r $ indicesByRelation inOutState

offsetForRelationAndIndex :: MonadReader InOutState m => Relation -> Index -> m Int
offsetForRelationAndIndex r idx = do
  inOutState <- ask
  pure . fromJust . M.lookup (r, idx) $ offsetsByRelationAndIndex inOutState

numColsForRelation :: MonadReader InOutState m => Relation -> m Int
numColsForRelation r =
  fromJust . M.lookup r . relationNumColumns <$> ask

switchOnFactType :: MonadFix m
                 => Set Relation
                 -> Map Relation Word32
                 -> IRBuilderT m ()
                 -> Operand
                 -> (Relation -> IRBuilderT m ())
                 -> IRBuilderT m ()
switchOnFactType rels stringMap defaultCase factType generateCase = mdo
    switch factType end caseBlocks
    caseBlocks <- for (M.toList relMapping) $ \(r, factNum) -> do
      caseBlock <- blockNamed (unId r)
      generateCase r
      pure (int32 $ toInteger factNum, caseBlock)

    end <- blockNamed "switch.default"
    defaultCase
  where
    relMapping =
      M.restrictKeys stringMap rels

-- A helper function for easily finding the "longest" index.
-- This is important for when you need to retrieve facts, since this index will
-- contain the most facts.
findLongestIndex :: [Index] -> Maybe Index
findLongestIndex =
  -- TODO use NonEmpty
  viaNonEmpty head . sortOn (Dual . length . unIndex)

isOutputRelation :: Map Relation UsageMode -> Relation -> Bool
isOutputRelation usageMapping r =
  case M.lookup r usageMapping of
    Just Output -> True
    Just InputOutput -> True
    _ -> False

isInputRelation :: Map Relation UsageMode -> Relation -> Bool
isInputRelation usageMapping r =
  case M.lookup r usageMapping of
    Just Input -> True
    Just InputOutput -> True
    _ -> False

getSymbolTablePtr :: (MonadModuleBuilder m, MonadIRBuilder m)
                  => Operand -> m Operand
getSymbolTablePtr program =
  gep program [int32 0, int32 0]

apiFunction :: (MonadModuleBuilder m, HasSuffix m)
            => Name
            -> [(LLVM.Type, ParameterName)]
            -> LLVM.Type
            -> ([Operand] -> IRBuilderT m a)
            -> m Operand
apiFunction fnName args retTy body =
  withFunctionAttributes (WasmExportName (unName fnName):) $
    function fnName args retTy body
