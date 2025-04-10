module Page.Test.Coordinates (tests) where

import Page.Constants (block_size, quad_size)
import Page.Geometry.Coordinates
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Page.Geometry.Coordinates"
    [ testCase "absoluteCharCoord" testAbsoluteCharCoord1
    ]

testAbsoluteCharCoord1 :: Assertion
testAbsoluteCharCoord1 = do
  let pos = AbsoluteCharCoord 32 48
  assertEqual
    "absoluteCharCoord 1"
    (AbsoluteTileCoord 0 0, TileRelativeCharCoord 32 48)
    (absoluteCharCoord pos)

-- TODO: add more tests
