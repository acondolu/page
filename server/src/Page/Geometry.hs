module Page.Geometry
  ( -- * Direction
    Direction (..),
    directionToOffset,
    offsetToDirection,
    opposite,

    -- * Geometric Figures
    Point (..),
    Rect (..),
    Pinned (..),

    -- * Area
    Area (..),
    belongsTo,
    overlaps,
    rdiff,
    nullA,
    relativeA,
    unrelative,
    restrict,
    intersect,

    -- * re-exports
    Word8,
    Word16,
  )
where

import Data.Maybe (mapMaybe)
import Data.Word (Word16, Word8)

-- | Eight cardinal and intercardinal directions.
data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Show, Enum, Bounded)

-- | Convert Direction to coordinate offsets (dx, dy)
directionToOffset :: Direction -> (Int, Int)
directionToOffset North = (0, -1)
directionToOffset NorthEast = (1, -1)
directionToOffset East = (1, 0)
directionToOffset SouthEast = (1, 1)
directionToOffset South = (0, 1)
directionToOffset SouthWest = (-1, 1)
directionToOffset West = (-1, 0)
directionToOffset NorthWest = (-1, -1)

-- | Convert coordinate offset to Direction
offsetToDirection :: Int -> Int -> Maybe Direction
offsetToDirection 0 (-1) = Just North
offsetToDirection 1 (-1) = Just NorthEast
offsetToDirection 1 0 = Just East
offsetToDirection 1 1 = Just SouthEast
offsetToDirection 0 1 = Just South
offsetToDirection (-1) 1 = Just SouthWest
offsetToDirection (-1) 0 = Just West
offsetToDirection (-1) (-1) = Just NorthWest
offsetToDirection _ _ = Nothing

opposite :: Direction -> Direction
opposite North = South
opposite NorthEast = SouthWest
opposite East = West
opposite SouthEast = NorthWest
opposite South = North
opposite SouthWest = NorthEast
opposite West = East
opposite NorthWest = SouthEast

-- | Point having (x, y) coordinates.
data Point a = Point a a
  deriving (Eq, Show)

-- | Rectangle having x1, y1, x2, y2 coordinates.
data Rect a = Rect a a a a
  deriving (Eq, Show)

-- | An object pinned on a point on a plane.
-- The object has type @b@, and the point has coordinates
-- (x, y) of type @a@.
-- It is isomorphic to:
-- >>> data Pinned a b = Pinned (Point a) b
data Pinned a b = Pinned a a b
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Area

-- | An area here is a union of rectangles on the 2D plane.
newtype Area a = Area [Rect a]
  deriving (Eq, Show)

-- | Difference of two rectangles.
rdiff :: (Ord a) => Rect a -> Rect a -> Area a
rdiff r@(Rect x1 y1 x2 y2) (Rect x1' y1' x2' y2') = do
  let xL = max x1 x1'
      xR = min x2 x2'
      yB = max y1 y1'
      yT = min y2 y2'
  if xL > xR || yB > yT
    then -- no overlap
      Area [r]
    else
      cleanup
        [ Rect x1 y1 xL y2, -- left
          Rect xR y1 x2 y2, -- right
          Rect xL y1 xR yB, -- bottom
          Rect xL yT xR y2 -- top
        ]

cleanup :: (Ord a) => [Rect a] -> Area a
cleanup rs = Area $ filter (not . nullR) rs

-- | Check if a rectangle is improper
-- (a point or has negative dimensions).
nullR :: (Ord a) => Rect a -> Bool
nullR (Rect x1 y1 x2 y2) =
  x2 <= x1 || y2 <= y1

-- | Check if area is empty.
nullA :: Area a -> Bool
nullA (Area rs) = null rs

-- | Restrict an area to a given rectangle.
restrict :: (Ord a) => Rect a -> Area a -> Area a
restrict rect (Area rs) = Area $ mapMaybe (intersect rect) rs

-- | Intersection of two rectangles. Return @Nothing@ if
-- no overlap.
intersect :: (Ord a) => Rect a -> Rect a -> Maybe (Rect a)
intersect (Rect x1 y1 x2 y2) (Rect x1' y1' x2' y2') =
  let xL = max x1 x1'
      xR = min x2 x2'
      yB = max y1 y1'
      yT = min y2 y2'
   in if xL <= xR && yB <= yT
        then Just (Rect xL yB xR yT)
        else Nothing

-- | Check if a point belongs to an area.
belongsTo :: (Ord a) => Point a -> Area a -> Bool
belongsTo (Point x y) (Area rs) = any go rs
  where
    go (Rect x1 y1 x2 y2) =
      x1 <= x && x <= x2 && y1 <= y && y <= y2
{-# SPECIALIZE belongsTo :: Point Word16 -> Area Word16 -> Bool #-}

-- | Check if a rectangle overlaps with an Area
overlaps :: (Ord a) => Rect a -> Area a -> Bool
overlaps (Rect x1 y1 x2 y2) (Area rs) = any go rs
  where
    go (Rect x3 y3 x4 y4) =
      x3 <= x2 && x1 <= x4 && y3 <= y2 && y1 <= y4

overlapsR :: (Ord a) => Rect a -> Rect a -> Bool
overlapsR (Rect x1 y1 x2 y2) (Rect x3 y3 x4 y4) =
  x3 <= x2 && x1 <= x4 && y3 <= y2 && y1 <= y4

cast16 :: Int -> Word16
cast16 n
  | n < 0 = 0
  | n > maxUint16 = maxBound
  | otherwise = fromIntegral n

maxUint16 :: Int
maxUint16 = fromIntegral (maxBound @Word16)

-- | Convert a rectangle to relative coordinates based on a point.
relativeR :: Point Int -> Rect Int -> Maybe (Rect Word16)
relativeR (Point px py) r@(Rect x1 y1 x2 y2)
  | overlapsR (Rect px py (px + maxUint16) (py + maxUint16)) r =
      Just $
        Rect
          (cast16 (x1 - px))
          (cast16 (y1 - py))
          (cast16 (x2 - px))
          (cast16 (y2 - py))
  | otherwise = Nothing

-- | Convert an area to relative coordinates based on a point.
-- Yields an empty area when the area is too far away from
-- the origin point.
relativeA :: Point Int -> Area Int -> Area Word16
relativeA p (Area rs) = Area $ mapMaybe (relativeR p) rs

-- | Kinda inverse of 'relativeA'.
unrelative :: Point Int -> Pinned Word16 a -> Pinned Int a
unrelative (Point x y) (Pinned x' y' a) =
  Pinned (x + fromIntegral x') (y + fromIntegral y') a
