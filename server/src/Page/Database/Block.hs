module Page.Database.Block
  ( Block,
    toByteString,
    newBlock,
    writeAt,
    fromString,
    bempty,
    bcount,
  )
where

import Control.Monad (when)
import Data.Binary (Binary (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke, pokeElemOff)
import Page.Constants
import Page.Geometry.Coordinates

size :: Int
size = block_size * block_size

-- | A square block of 256 ASCII characters
-- stored in pinned memory.
newtype Block = Block (ForeignPtr Word8)

instance Binary Block where
  put (Block p) = put (BS.BS p size)
  get = do
    BS.BS p n <- get
    when (n /= size) $
      fail "Binary.get: wrong number of bytes"
    pure (Block p)

instance Show Block where
  show = show . toByteString

instance Eq Block where
  Block p1 == Block p2 = BS.BS p1 size == BS.BS p2 size

-- | Create a Block from a String.
-- Warning: the String must be exactly 256 characters long.
-- If not, raises a pure exception.
fromString :: String -> Block
fromString s =
  case BS8.pack s of
    BS.BS p n
      | n == size -> Block p
      | otherwise -> error $ "Block.fromString: " <> show n

-- | Convert a Block to a ByteString for serialization.
toByteString :: Block -> BS.ByteString
toByteString (Block bs) = BS.BS bs size

-- | Create a new Block filled with spaces (ASCII 32).
newBlock :: IO Block
newBlock = do
  fp <- mallocForeignPtrBytes size
  let go :: Int -> Ptr Word8 -> IO ()
      go 0 !_ = return ()
      go i !p = poke p 32 >> go (i - 1) (p `plusPtr` 1)
  BS.unsafeWithForeignPtr fp $ go size
  pure (Block fp)

-- | Write the given byte to the given coordinates in a block.
-- Parameters are: block, x-coordinate, y-coordinate, and character to write.
writeAt :: Block -> BlockRelativeCharCoord -> Word8 -> IO ()
writeAt (Block fp) (BlockRelativeCharCoord x y) c = BS.unsafeWithForeignPtr fp $ \p ->
  pokeElemOff p (fromIntegral x + fromIntegral y * block_size) c

-- | Check if a block is empty (all spaces).
bempty :: Block -> Bool
bempty (Block p) = B.all (== 32) $ BS.BS p size

-- | Count the number of non-space characters in a block.
bcount :: Block -> Int
bcount (Block p) = B.length $ B.filter (/= 32) $ BS.BS p size
