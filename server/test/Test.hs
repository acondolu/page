module Main (main) where

import qualified Page.Test.Command
import qualified Page.Test.Coordinates
import qualified Page.Test.Cursor
import qualified Page.Test.Geometry
import qualified Page.Test.QuadTree
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Page.Test.Command.test,
      Page.Test.Geometry.test,
      Page.Test.QuadTree.test,
      Page.Test.Cursor.test,
      Page.Test.Coordinates.tests
    ]

main :: IO ()
main = defaultMain tests
