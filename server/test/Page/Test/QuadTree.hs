module Page.Test.QuadTree (test) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import qualified Data.Map as Map
import Page.QuadTree
import qualified Page.QuadTree as QuadTree
import System.Random (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

emp :: QuadTree Int
emp = empty

emptyLeaf :: Assertion
emptyLeaf =
  insert 0 0 42 emp @?= Leaf 0 0 42

leafNode1 :: Assertion
leafNode1 =
  Node {nw = Leaf 0 0 42, ne = Empty, sw = Leaf 0 32768 6, se = Empty}
    @=? (emp & insert 0 0 42 & insert 0 32768 6)

leafNode2 :: Assertion
leafNode2 =
  Node {nw = Node {nw = Leaf 0 0 42, ne = Empty, sw = Leaf 0 16384 6, se = Empty}, ne = Empty, sw = Empty, se = Empty}
    @=? (emp & insert 0 0 42 & insert 0 16384 6)

leafNode3 :: Assertion
leafNode3 =
  Node {nw = Node {nw = Node {nw = Leaf 0 0 42, ne = Empty, sw = Empty, se = Leaf 8192 8192 6}, ne = Empty, sw = Empty, se = Empty}, ne = Empty, sw = Empty, se = Empty}
    @=? (emp & insert 0 0 42 & insert 8192 8192 6)

insertLookup :: Assertion
insertLookup = do
  let max16 = maxBound :: Coord
  let randomCoord (n :: Int) = do
        x <- randomRIO (0, max16)
        y <- randomRIO (0, max16)
        pure (x, y, n)
  coords <- traverse randomCoord [0 .. 500]
  let tree = foldr (\(x, y, n) acc -> QuadTree.insert x y n acc) QuadTree.empty coords
  let check (x, y, n) =
        QuadTree.lookup x y tree @?= Just n
  traverse_ check coords

mapEquivalence :: Property
mapEquivalence = property $ do
  let max16 = maxBound :: Coord
  coords <- vectorOf 1000 $ (,,) <$> choose (0, max16) <*> choose (0, max16) <*> arbitrary
  coords' <- vectorOf 1000 $ (,,) <$> choose (0, max16) <*> choose (0, max16) <*> arbitrary
  let tree = foldr (\(x, y, n :: Int) acc -> QuadTree.insert x y n acc) QuadTree.empty coords
  let m = foldr (\(x, y, n) acc -> Map.insert (x, y) n acc) Map.empty coords
  pure $ all (\(x, y, _) -> QuadTree.lookup x y tree == Map.lookup (x, y) m) (coords <> coords')

test :: TestTree
test =
  testGroup
    "Page.QuadTree"
    [ testCase "empty-leaf" emptyLeaf,
      testCase "leaf-node-1" leafNode1,
      testCase "leaf-node-2" leafNode2,
      testCase "leaf-node-3" leafNode3,
      testCase "insert-lookup" insertLookup,
      QC.testProperty "map-equiv" mapEquivalence
    ]
