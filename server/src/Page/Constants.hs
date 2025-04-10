module Page.Constants
  ( block_size,
    quad_size,
  )
where

-- | A block is 16x16=256 ASCII characters.
block_size :: (Num a) => a
block_size = 16
{-# INLINE block_size #-}

-- | A quadtree is 2^16 x 2^16 blocks.
quad_size :: (Num a) => a
quad_size = 65536
{-# INLINE quad_size #-}
