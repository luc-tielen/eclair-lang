module Eclair.LLVM.Vector
  ( Vector(..)
  , Types(..)
  , Destructor
  , codegen
  , startPtrOf
  ) where

import Prelude hiding (EQ, void)
import Control.Monad.Morph
import Eclair.LLVM.Runtime
import Eclair.LLVM.Codegen

data Types
  = Types
  { tyIndex :: Type
  , tyElement :: Type
  , tyVector :: Type
  }

data Vector
  = Vector
  { vectorTypes :: Types
  , vectorInit :: Operand
  , vectorDestroy :: Operand
  , vectorPush :: Operand
  , vectorSize :: Operand
  , vectorGetValue :: Operand
  }

-- Type representing what to do when an element of the vector is destroyed.
-- The operand is a pointer to an element type that needs to be cleaned up.
type Destructor = Operand -> IRCodegen ()

data CGState
  = CGState
  { externals :: Externals
  , types :: Types
  , sizeElement :: Word64
  , destructor :: Maybe Destructor
  }

type ModuleCodegen = ReaderT CGState (Template Type)
type IRCodegen = IRBuilderT ModuleCodegen


codegen :: Externals -> Maybe Destructor -> TemplateT Type IO Vector
codegen exts dtor = do
  tyElem <- getParams
  sizeOfElem <- withLLVMTypeInfo $ \ctx td -> llvmSizeOf ctx td tyElem

  hoist intoIO $ do
    tys <- generateTypes
    runReaderT generateFunctions $ CGState exts tys sizeOfElem dtor
  where
    intoIO = pure . runIdentity

generateTypes :: Template Type Types
generateTypes = do
  tyElem <- getParams
  tyVec <- typedef "vector_t" Off
            [ ptr tyElem  -- pointer to start of the vector
            , ptr tyElem  -- pointer to one element past end of the vector
            , i32         -- capacity: how many elements can fit inside the vector
            ]
  pure $ Types
    { tyIndex = i32
    , tyElement = tyElem
    , tyVector = tyVec
    }

generateFunctions :: ModuleCodegen Vector
generateFunctions = do
  tys <- asks types
  vInit <- mkVectorInit
  vDestroy <- mkVectorDestroy
  vSize <- mkVectorSize
  vPush <- mkVectorPush vSize
  vGetValue <- mkVectorGetValue
  pure $ Vector
    { vectorTypes = tys
    , vectorInit = vInit
    , vectorDestroy = vDestroy
    , vectorPush = vPush
    , vectorSize = vSize
    , vectorGetValue = vGetValue
    }

initialCapacity :: Int
initialCapacity = 16  -- or 0?

growFactor :: Int
growFactor = 2  -- or 1.5? needs rounding then..

-- NOTE: Assumes vector memory already allocated in other code
mkVectorInit :: ModuleCodegen Operand
mkVectorInit = do
  CGState exts tys sizeOfElem _ <- ask
  let (vecTy, elemTy) = (tyVector &&& tyElement) tys
      mallocFn = extMalloc exts

  function "vector_init" [(ptr vecTy, "vec")] void $ \[vec] -> do
    -- assert(vec && "Vector should not be null");
    let numBytes = int32 . toInteger $ sizeOfElem * fromIntegral initialCapacity
    memoryPtr <- (`bitcast` ptr elemTy) =<< call mallocFn [numBytes]
    --   assert(memory && "Failed to allocate memory!");

    assign startPtrOf vec memoryPtr
    assign endPtrOf vec memoryPtr
    assign capacityOf vec (int32 $ fromIntegral initialCapacity)

-- NOTE: Assumes vector memory already allocated in other code
mkVectorDestroy ::  ModuleCodegen Operand
mkVectorDestroy = do
  CGState exts tys _ elemDestructor <- ask
  let (vecTy, elemTy) = (tyVector &&& tyElement) tys
      freeFn = extFree exts

  function "vector_destroy" [(ptr vecTy, "vec")] void $ \[vec] -> do
    -- assert(vec && "Vector should not be null");
    for_ elemDestructor $ \destructor -> do
      iterPtrPtr <- allocate (ptr elemTy) =<< deref startPtrOf vec
      let hasNext = do
            iterPtr <- load iterPtrPtr 0
            endPtr <- deref endPtrOf vec
            iterPtr `ne` endPtr
      loopWhile hasNext $ do
        iterPtr <- load iterPtrPtr 0
        destructor iterPtr
        store iterPtrPtr 0 =<< incrementPtr iterPtr

    startPtr <- (`bitcast` ptr i8) =<< deref startPtrOf vec
    call freeFn [startPtr]

-- NOTE: Returns the index at which the element was inserted => no size necessary
-- NOTE: does not check for uniqueness!
mkVectorPush :: Operand -> ModuleCodegen Operand
mkVectorPush vectorSize = do
  CGState exts tys sizeElem _ <- ask
  let (vecTy, elemTy) = (tyVector &&& tyElement) tys
      mallocFn = extMalloc exts
      freeFn = extFree exts
      memcpyFn = extMemcpy exts
      sizeOfElem = int32 $ toInteger sizeElem

  vectorGrow <- function "vector_grow" [(ptr vecTy, "vec")] void $ \[vec] -> do
    -- NOTE: size == capacity in this function
    -- assert(vec && "Vector should not be null");
    currentCapacity <- deref capacityOf vec
    currentNumBytes <- mul currentCapacity sizeOfElem >>= (`zext` i64)

    newCapacity <- mul currentCapacity (int32 $ toInteger growFactor)
    newNumBytes <- mul newCapacity sizeOfElem
    newMemoryPtr <- (`bitcast` ptr elemTy) =<< call mallocFn [newNumBytes]
    -- assert(new_memory && "Failed to allocate more memory for vector!");
    newMemoryEndPtr <- gep newMemoryPtr [newCapacity]  -- TODO check
    startPtr <- deref startPtrOf vec >>= (`bitcast` ptr i8)
    newMemoryPtrBytes <- newMemoryPtr `bitcast` ptr i8
    call memcpyFn [newMemoryPtrBytes, startPtr, currentNumBytes, bit 0]
    call freeFn [startPtr]

    assign startPtrOf vec newMemoryPtr
    assign endPtrOf vec newMemoryEndPtr
    assign capacityOf vec newCapacity

  function "vector_push" [(ptr vecTy, "vec"), (ptr elemTy, "elem")] i32 $ \[vec, elem] -> do
    -- assert(vec && "Vector should not be null");
    numElems <- call vectorSize [vec]
    capacity <- deref capacityOf vec
    isFull <- numElems `eq` capacity
    if' isFull $ do
      call vectorGrow [vec]

    -- Look up vec->end again, pointers can be invalidated due to potential resize!
    endPtr <- deref endPtrOf vec
    store endPtr 0 =<< load elem 0
    update endPtrOf vec incrementPtr
    ret numElems

mkVectorSize :: ModuleCodegen Operand
mkVectorSize = do
  CGState _ tys sizeElem _ <- ask
  let vecTy = tyVector tys
      sizeOfElem = int32 $ toInteger sizeElem

  function "vector_size" [(ptr vecTy, "vec")] i32 $ \[vec] -> do
    -- assert(vec && "Vector should not be null");
    startPtr <- deref startPtrOf vec
    endPtr <- deref endPtrOf vec
    byteDiff <- pointerDiff i32 startPtr endPtr
    ret =<< udiv byteDiff sizeOfElem

mkVectorGetValue :: ModuleCodegen Operand
mkVectorGetValue = do
  (vecTy, elemTy) <- asks ((tyVector &&& tyElement) . types)
  function "vector_get_value" [(ptr vecTy, "vec"), (i32, "idx")] (ptr elemTy) $ \[vec, idx] -> do
    startPtr <- deref startPtrOf vec
    -- We need a raw gep here, since this is a dynamically allocated pointer that we need to offset.
    ret =<< gep startPtr [idx]


-- Helper functions:

incrementPtr :: Operand -> IRCodegen Operand
incrementPtr = (`gep` [int32 1])

data Index
  = VectorIdx
  | StartPtrIdx
  | EndPtrIdx
  | CapacityIdx

startPtrOf :: Path 'VectorIdx 'StartPtrIdx
startPtrOf = mkPath [int32 0]

endPtrOf :: Path 'VectorIdx 'EndPtrIdx
endPtrOf = mkPath [int32 1]

capacityOf :: Path 'VectorIdx 'CapacityIdx
capacityOf = mkPath [int32 2]
