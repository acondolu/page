module Page.Test.Cursor (test) where

import qualified Page.Database as Database
import Page.Database.Block (fromString)
import qualified Page.Database.Cursor as Cursor
import Page.Geometry
import Page.Geometry.Coordinates
import Test.Tasty
import Test.Tasty.HUnit
import Page.Constants

test :: TestTree
test =
  testGroup
    "Page.Test.Cursor"
    [ testCase "test1" test1,
      testCase "test1'" test1',
      testCase "test2" test2,
      testCase "test3" test3,
      testCase "test4" test4,
      testCase "test5" test5
    ]

new :: IO Cursor.Cursor
new = do
  db <- Database.new
  Cursor.origin db

test1 :: Assertion
test1 = do
  cur <- new
  Cursor.writeCharAt cur (TileRelativeCharCoord' 0 0) '@'
  blocks <- Cursor.query cur (Area [Rect 0 0 1 1])
  case blocks of
    [Pinned 0 0 (Database.Block _ cs)] -> do
      let str = fromString $ '@' : replicate 255 ' '
      cs @?= str
    _ -> assertFailure "Unexpected query result"

test1' :: Assertion
test1' = do
  cur <- new
  Cursor.writeCharAt cur (TileRelativeCharCoord' 65535 0) '@'
  blocks <- Cursor.query cur (Area [Rect 0 0 65791 65791])
  case blocks of
    [Pinned 4095 0 (Database.Block _ cs)] -> do
      let str = fromString $ replicate 15 ' ' ++ "@" ++ replicate 240 ' '
      cs @?= str
    _ -> do
      print blocks
      assertFailure "Unexpected query result"

test2 :: Assertion
test2 = do
  cur <- new
  Cursor.writeStringAt cur (TileRelativeCharCoord' 0 0) "hello"
  Cursor.writeStringAt cur (TileRelativeCharCoord' 1 0) "world"
  blocks <- Cursor.query cur (Area [Rect 0 0 1 5])
  case blocks of
    [Pinned 0 0 (Database.Block _ cs)] -> do
      let str = fromString $ "hworld" ++ replicate 250 ' '
      cs @?= str
    _ -> assertFailure "Unexpected query result"

test3 :: Assertion
test3 = do
  cur <- new
  Cursor.writeStringAt cur (TileRelativeCharCoord' 0 0) "hello"
  Cursor.writeStringAt cur (TileRelativeCharCoord' 256 0) "world"
  blocks <- Cursor.query cur (Area [Rect 0 0 512 512])
  case blocks of
    [Pinned 0 0 (Database.Block _ cs0), Pinned 16 0 (Database.Block _ cs1)] -> do
      let str0 = fromString $ "hello" ++ replicate 251 ' '
          str1 = fromString $ "world" ++ replicate 251 ' '
      cs0 @?= str0
      cs1 @?= str1
    _ -> do
      print blocks
      assertFailure "Unexpected query result"

test4 :: Assertion
test4 = do
  cur <- new
  Cursor.writeStringAt cur (TileRelativeCharCoord' 0 0) "hello"
  Cursor.writeStringAt cur (TileRelativeCharCoord' 65535 0) "world"
  blocks <- Cursor.query cur (Area [Rect 0 0 65791 65791])
  case blocks of
    [ Pinned 0 0 (Database.Block _ cs0),
      Pinned 4095 0 (Database.Block _ cs1),
      Pinned 4096 0 (Database.Block _ cs2)
      ] -> do
        let str0 = fromString $ "hello" ++ replicate 251 ' '
            str1 = fromString $ replicate 15 ' ' ++ "w" ++ replicate 240 ' '
            str2 = fromString $ "orld" ++ replicate 252 ' '
        cs0 @?= str0
        cs1 @?= str1
        cs2 @?= str2
    _ -> do
      print blocks
      assertFailure "Unexpected query result"

-- | Relative projections over neighbouring tiles.
test5 :: Assertion
test5 = do
  let dir d = do
        let (dx, dy) = directionToOffset d
        let offset = Point (dx * quad_size) (dy * quad_size)
        relativeA offset
  let a1 = Area [Rect (-1) (-1) (10) (10)]
  dir West a1 @?= Area [Rect 65535 0 65535 10]
  dir North a1 @?= Area [Rect 0 65535 10 65535]
  dir NorthWest a1 @?= Area [Rect 65535 65535 65535 65535]
  relativeA (Point 0 0) a1 @?= Area [Rect 0 0 10 10]
