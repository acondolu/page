{-# LANGUAGE ViewPatterns #-}

module Page.Geometry.Coordinates
  ( AbsoluteCharCoord (..),
    AbsoluteTileCoord (..),
    TileRelativeCharCoord (..),
    TileRelativeCharCoord' (..),
    QuadRelativeBlockCoord (..),
    BlockRelativeCharCoord (..),
    TileRelativeTileCoord (..),
    absoluteCharCoord,
    tileRelativeCharCoord,
    tileRelativeCharCoord',
  )
where

import Data.Word (Word16, Word32, Word8)
import Page.Constants (block_size, quad_size)

-- | Global position of a character on the infinite plane
data AbsoluteCharCoord = AbsoluteCharCoord Integer Integer
  deriving (Eq, Show)

-- | Global position of a tile on the plane
data AbsoluteTileCoord = AbsoluteTileCoord Integer Integer
  deriving (Eq, Show)

-- | Character position relative to a tile's origin
data TileRelativeCharCoord = TileRelativeCharCoord Word32 Word32
  deriving (Eq, Show)

data TileRelativeCharCoord' = TileRelativeCharCoord' Int Int
  deriving (Eq, Show)

absoluteCharCoord :: AbsoluteCharCoord -> (AbsoluteTileCoord, TileRelativeCharCoord)
absoluteCharCoord (AbsoluteCharCoord x y) = do
  let (xd, fromInteger -> xm) = x `divMod` (block_size * quad_size)
      (yd, fromInteger -> ym) = y `divMod` (block_size * quad_size)
  (AbsoluteTileCoord xd yd, TileRelativeCharCoord xm ym)

-- | Position of a block within a QuadTree.
data QuadRelativeBlockCoord = QuadRelativeBlockCoord Word16 Word16
  deriving (Eq, Show)

-- | Character position within a block.
data BlockRelativeCharCoord = BlockRelativeCharCoord Word8 Word8
  deriving (Eq, Show)

tileRelativeCharCoord :: TileRelativeCharCoord -> (QuadRelativeBlockCoord, BlockRelativeCharCoord)
tileRelativeCharCoord (TileRelativeCharCoord x y) = do
  let (bx, fromIntegral -> cx) = x `divMod` block_size
      (by, fromIntegral -> cy) = y `divMod` block_size
      (tx, fromIntegral -> qx) = bx `divMod` quad_size
      (ty, fromIntegral -> qy) = by `divMod` quad_size
  if tx == 0 && ty == 0
    then (QuadRelativeBlockCoord qx qy, BlockRelativeCharCoord cx cy)
    else error "tileRelativeCharCoord: impossible"

data TileRelativeTileCoord = TileRelativeTileCoord Int Int
  deriving (Eq, Show)

tileRelativeCharCoord' :: TileRelativeCharCoord' -> (TileRelativeTileCoord, QuadRelativeBlockCoord, BlockRelativeCharCoord)
tileRelativeCharCoord' (TileRelativeCharCoord' x y) = do
  let (bx, fromIntegral -> cx) = x `divMod` block_size
      (by, fromIntegral -> cy) = y `divMod` block_size
      (tx, fromIntegral -> qx) = bx `divMod` quad_size
      (ty, fromIntegral -> qy) = by `divMod` quad_size
  (TileRelativeTileCoord tx ty, QuadRelativeBlockCoord qx qy, BlockRelativeCharCoord cx cy)
