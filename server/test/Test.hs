module Main (main) where

import qualified Page.Test.Command
import qualified Page.Test.Coordinates
import qualified Page.Test.Cursor
import qualified Page.Test.Geometry
import qualified Page.Test.QuadTree
import qualified Page.Test.RingBuffer
import qualified Page.Test.StrokeLimiter
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Page.Test.Command.test,
      Page.Test.Geometry.test,
      Page.Test.QuadTree.test,
      Page.Test.Cursor.test,
      Page.Test.Coordinates.tests,
      Page.Test.RingBuffer.test,
      Page.Test.StrokeLimiter.test
    ]

main :: IO ()
main = defaultMain tests
