module Page.RingBuffer
  ( RingBuffer,
    bufNew,
    bufPush,
    bufReplaceLast,
    toList,
  )
where

import Control.Concurrent.STM
import Control.Monad (forM, when)
import Data.Array.MArray (newArray_, readArray, writeArray)

data RingBuffer a = RingBuffer
  { buffer :: TArray Int a,
    capacity :: Int,
    start :: TVar Int,
    size :: TVar Int
  }

bufNew :: Int -> IO (RingBuffer a)
bufNew cap = atomically $ do
  buf <- newArray_ (0, cap - 1)
  startRef <- newTVar 0
  sizeRef <- newTVar 0
  pure $ RingBuffer buf cap startRef sizeRef

bufPush :: RingBuffer a -> a -> IO ()
bufPush RingBuffer {..} x = atomically $ do
  s <- readTVar start
  sz <- readTVar size
  writeArray buffer ((s + sz) `mod` capacity) x
  if sz < capacity
    then writeTVar size (sz + 1)
    else writeTVar start ((s + 1) `mod` capacity)

bufReplaceLast :: RingBuffer a -> a -> IO ()
bufReplaceLast RingBuffer {..} x = atomically $ do
  s <- readTVar start
  sz <- readTVar size
  when (sz > 0) $ writeArray buffer ((s + sz - 1) `mod` capacity) x

toList :: RingBuffer a -> IO [a]
toList RingBuffer {..} = atomically $ do
  s <- readTVar start
  sz <- readTVar size
  forM [0 .. sz - 1] $ \i -> readArray buffer ((s + i) `mod` capacity)
