module Page.RingBuffer
  ( RingBuffer,
    new,
    push,
    replaceLast,
    toList,
  )
where

import Control.Concurrent.STM
import Control.Monad (forM, when)
import Data.Array.MArray (newArray_, readArray, writeArray)

-- | A simple ring buffer:
-- - fixed capacity
-- - thread-safe (via STM)
data RingBuffer a = RingBuffer
  { buffer :: TArray Int a,
    capacity :: Int,
    start :: TVar Int,
    size :: TVar Int
  }

new :: Int -> IO (RingBuffer a)
new cap = atomically $ do
  buf <- newArray_ (0, cap - 1)
  startRef <- newTVar 0
  sizeRef <- newTVar 0
  pure $ RingBuffer buf cap startRef sizeRef

push :: RingBuffer a -> a -> STM ()
push RingBuffer {..} x = do
  s <- readTVar start
  sz <- readTVar size
  writeArray buffer ((s + sz) `mod` capacity) x
  if sz < capacity
    then writeTVar size (sz + 1)
    else writeTVar start ((s + 1) `mod` capacity)

replaceLast :: RingBuffer a -> a -> STM ()
replaceLast RingBuffer {..} x = do
  s <- readTVar start
  sz <- readTVar size
  when (sz > 0) $ writeArray buffer ((s + sz - 1) `mod` capacity) x

toList :: RingBuffer a -> STM [a]
toList RingBuffer {..} = do
  s <- readTVar start
  sz <- readTVar size
  forM [0 .. sz - 1] $ \i -> readArray buffer ((s + i) `mod` capacity)
