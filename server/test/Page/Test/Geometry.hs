module Page.Test.Geometry (test) where

import Page.Geometry
import Test.Tasty
import Test.Tasty.HUnit

rect1 :: Rect Integer
rect1 = Rect 3 5 7 11

rect2 :: Rect Integer
rect2 = Rect 5 7 11 13

rect3 :: Rect Integer
rect3 = Rect (-1) (-1) 1 1

rect4 :: Rect Integer
rect4 = Rect (-3) (-3) 3 3

test1 :: Assertion
test1 =
  Area [Rect 3 5 5 11, Rect 5 5 7 7]
    @=? (rect1 `rdiff` rect2)

test2 :: Assertion
test2 =
  Area [Rect 7 7 11 13, Rect 5 11 7 13]
    @=? (rect2 `rdiff` rect1)

test3 :: Assertion
test3 =
  Area [Rect (-3) (-3) (-1) 3, Rect 1 (-3) 3 3, Rect (-1) (-3) 1 (-1), Rect (-1) 1 1 3]
    @=? (rect4 `rdiff` rect3)

test4 :: Assertion
test4 =
  Area [] @=? (rect3 `rdiff` rect4)

test5 :: Assertion
test5 =
  Area [] @=? (rect3 `rdiff` rect3)

test6 :: Assertion
test6 =
  Area [] @=? (rect4 `rdiff` rect4)

test7 :: Assertion
test7 =
  Just (Rect 5 7 7 11) @=? (rect1 `intersect` rect2)

test :: TestTree
test =
  testGroup
    "Page.Geometry"
    [ testCase "test1" test1,
      testCase "test2" test2,
      testCase "test3" test3,
      testCase "test4" test4,
      testCase "test5" test5,
      testCase "test6" test6,
      testCase "test7" test7
    ]
