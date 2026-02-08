module Page.Test.RingBuffer (test) where

import Control.Concurrent.STM (atomically)
import Page.RingBuffer (RingBuffer, new, push, replaceLast, toList)
import Test.Tasty
import Test.Tasty.HUnit hiding ((@=?))

list :: RingBuffer a -> IO [a]
list = atomically . toList

(@=?) :: (Eq a, Show a) => [a] -> RingBuffer a -> Assertion
expected @=? rb = do
  actual <- list rb
  actual @?= expected

test :: TestTree
test =
  testGroup
    "Page.RingBuffer"
    [ testCase "new buffer is empty" $ do
        rb <- new @Int 3
        [] @=? rb,
      testCase "push one" $ do
        rb <- new @Int 3
        atomically $ push rb 1
        [1] @=? rb,
      testCase "push to capacity" $ do
        rb <- new @Int 3
        atomically $ push rb 1 >> push rb 2 >> push rb 3
        [1, 2, 3] @=? rb,
      testCase "push past capacity drops oldest" $ do
        rb <- new @Int 3
        atomically $ push rb 1 >> push rb 2 >> push rb 3 >> push rb 4
        [2, 3, 4] @=? rb,
      testCase "push wraps multiple times" $ do
        rb <- new @Int 2
        atomically $ push rb 1 >> push rb 2 >> push rb 3 >> push rb 4 >> push rb 5
        [4, 5] @=? rb,
      testCase "replaceLast on empty is no-op" $ do
        rb <- new @Int 3
        atomically $ replaceLast rb 99
        [] @=? rb,
      testCase "replaceLast replaces last pushed" $ do
        rb <- new @Int 3
        atomically $ push rb 1 >> push rb 2 >> replaceLast rb 99
        [1, 99] @=? rb,
      testCase "replaceLast after wrap" $ do
        rb <- new @Int 2
        atomically $ push rb 1 >> push rb 2 >> push rb 3 >> replaceLast rb 99
        [2, 99] @=? rb,
      testCase "capacity 1" $ do
        rb <- new @Int 1
        atomically $ push rb 1
        [1] @=? rb
        atomically $ push rb 2
        [2] @=? rb
        atomically $ replaceLast rb 99
        [99] @=? rb
    ]
