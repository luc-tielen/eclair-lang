{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.EIR.Lower.Externals
  ( createExternals
  ) where

import Prelude hiding (void)
import Eclair.EIR.Lower.Codegen
import Eclair.LLVM.Codegen as LLVM
import Eclair.Common.Config


createExternals :: ConfigT (ModuleBuilderT IO) Externals
createExternals = do
  target <- cfgTargetTriple <$> getConfig
  mallocFn <- lift $ generateMallocFn target
  freeFn <- lift $ generateFreeFn target
  memsetFn <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  memcpyFn <- extern "llvm.memcpy.p0i8.p0i8.i64" [ptr i8, ptr i8, i64, i1] void
  memcmpFn <- if target == Just Wasm32
                then lift generateMemCmpFn
                else extern "memcmp" [ptr i8, ptr i8, i64] i32
  -- TODO write alternatives for WASM
  mmapFn <- extern "mmap" [ptr void, i64, i32, i32, i32, i32] (ptr void)
  munmapFn <- extern "munmap" [ptr void, i64] i32
  pure $ Externals mallocFn freeFn memsetFn memcpyFn memcmpFn mmapFn munmapFn

generateMallocFn :: Monad m => Maybe Target -> ModuleBuilderT m Operand
generateMallocFn target = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  when (target == Just Wasm32) $ do
    _ <- withFunctionAttributes (const [WasmExportName "eclair_malloc"]) $
      function "eclair_malloc" [(i32, "byte_count")] (ptr i8) $ \[byteCount] ->
        ret =<< call mallocFn [byteCount]
    pass

  pure mallocFn

generateFreeFn :: Monad m => Maybe Target -> ModuleBuilderT m Operand
generateFreeFn target = do
  freeFn <- extern "free" [ptr i8] void
  when (target == Just Wasm32) $ do
    _ <- withFunctionAttributes (const [WasmExportName "eclair_free"]) $
      function "eclair_free" [(ptr i8, "memory")] void $ \[memoryPtr] ->
        call freeFn [memoryPtr]
    pass

  pure freeFn

-- NOTE: we only care about 0 if they are equal!
generateMemCmpFn :: MonadFix m => ModuleBuilderT m Operand
generateMemCmpFn = do
  let args = [(ptr i8, "array1"), (ptr i8, "array2"), (i64, "byte_count")]
  function "memcmp_wasm32" args i32 $ \[array1, array2, byteCount] -> do
    i64Count <- byteCount `udiv` int64 8
    restCount <- byteCount `and` int64 7  -- modulo 8
    let i64Array1 = ptrcast i64 array1
    let i64Array2 = ptrcast i64 array2

    loopFor (int64 0) (`ult` i64Count) (add (int64 1)) $ \i -> do
      valuePtr1 <- gep i64Array1 [i]
      valuePtr2 <- gep i64Array2 [i]
      value1 <- load valuePtr1 0
      value2 <- load valuePtr2 0

      isNotEqual <- value1 `ne` value2
      if' isNotEqual $
        ret $ int32 1

    startIdx <- mul i64Count (int64 8)
    loopFor (int64 0) (`ult` restCount) (add (int64 1)) $ \i -> do
      idx <- add i startIdx
      valuePtr1 <- gep array1 [idx]
      valuePtr2 <- gep array2 [idx]
      value1 <- load valuePtr1 0
      value2 <- load valuePtr2 0

      isNotEqual <- value1 `ne` value2
      if' isNotEqual $
        ret $ int32 1

    ret $ int32 0
