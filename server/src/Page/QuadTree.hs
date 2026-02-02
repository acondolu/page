module Page.QuadTree
  ( QuadTree (..),
    Coord,
    empty,
    insert,
    query,
    lookup,
    toList,
  )
where

import Data.Bits
import Data.Function ((&))
import Data.Word (Word16)
import Page.Geometry (Pinned (..))
import qualified Page.Geometry as Geometry
import Prelude hiding (lookup)

-- | Coordinate in the quadtree (16-bit).
type Coord = Word16

half :: Coord
half = 1 `shiftL` (finiteBitSize (0 :: Coord) - 1)

-- | Usual quadtree data structure. It recursively partitions the
-- 16-bit space into quadrants, for efficient insertion
-- and query.
data QuadTree a
  = Empty
  | Leaf {x :: Coord, y :: Coord, a :: a}
  | Node {nw :: QuadTree a, ne :: QuadTree a, sw :: QuadTree a, se :: QuadTree a}
  deriving (Show, Eq)

empty :: QuadTree a
empty = Empty

insert :: (Show a) => Coord -> Coord -> a -> QuadTree a -> QuadTree a
insert = insert' half

insert' :: (Show a) => Coord -> Coord -> Coord -> a -> QuadTree a -> QuadTree a
insert' _ x y !a Empty = Leaf x y a
insert' 0 x y !a node = error $ show ("insert'" :: String, x, y, a, node)
insert' n x y !a (Leaf x' y' a') =
  Node Empty Empty Empty Empty
    & insert' n x' y' a'
    & insert' n x y a
insert' n x y !a Node {..} = do
  let n' = n `shiftR` 1
  if x .&. n == 0
    then
      if y .&. n == 0
        then Node {nw = insert' n' x y a nw, ..}
        else Node {sw = insert' n' x y a sw, ..}
    else
      if y .&. n == 0
        then Node {ne = insert' n' x y a ne, ..}
        else Node {se = insert' n' x y a se, ..}

lookup :: Coord -> Coord -> QuadTree a -> Maybe a
lookup = go half
  where
    go _ _ _ Empty = Nothing
    go _ x' y' Leaf {..}
      | x' == x && y' == y = Just a
      | otherwise = Nothing
    go n x y Node {..} = do
      let n' = n `shiftR` 1
      if x .&. n == 0
        then
          if y .&. n == 0
            then go n' x y nw
            else go n' x y sw
        else
          if y .&. n == 0
            then go n' x y ne
            else go n' x y se

query :: QuadTree a -> Geometry.Area Coord -> [Pinned Coord a]
query qt area
  | Geometry.nullA area = []
  | otherwise = query' 0 0 maxBound maxBound half qt area []

query' :: Coord -> Coord -> Coord -> Coord -> Coord -> QuadTree a -> Geometry.Area Coord -> [Pinned Coord a] -> [Pinned Coord a]
query' _ _ _ _ _ Empty _ = id
query' _ _ _ _ _ (Leaf {x, y, a}) area
  | Geometry.Point x y `Geometry.belongsTo` area = (:) (Pinned x y a)
  | otherwise = id
query' x1 y1 x2 y2 n Node {..} a = do
  let n' = n `shiftR` 1
      n1 = n - 1
  recurseInto x1 y1 (x1 + n1) (y1 + n1) n' nw a
    . recurseInto (x1 + n) y1 x2 (y1 + n1) n' ne a
    . recurseInto x1 (y1 + n) (x1 + n1) y2 n' sw a
    . recurseInto (x1 + n) (y1 + n) x2 y2 n' se a

recurseInto :: Word16 -> Word16 -> Word16 -> Word16 -> Coord -> QuadTree a -> Geometry.Area Word16 -> [Pinned Coord a] -> [Pinned Coord a]
recurseInto !x1 !y1 !x2 !y2 !n' qt a = do
  let a' = Geometry.restrict (Geometry.Rect x1 y1 x2 y2) a
  if Geometry.nullA a'
    then id
    else query' x1 y1 x2 y2 n' qt a'

toList :: QuadTree a -> [(Coord, Coord, a)]
toList t = go t mempty
  where
    go Empty = id
    go (Leaf {a, x, y}) = (:) (x, y, a)
    go Node {..} = go nw . go ne . go sw . go se
