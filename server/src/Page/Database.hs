{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | In-memory database for managing the distributed collaborative document.
-- Organizes content into blocks stored in cells within a coordinate system.
module Page.Database
  ( -- * Block
    Block (..),
    mkBlock,
    blockToTexts,
    Revision,
    writeAt,

    -- * Cell
    Cell (..),
    DB (..),
    DB',
    new,
    revision,
    getTileForce,
    mkTile,

    -- * Locking
    Locked (..),
    Lock,
    getLock,
    withLock,
    withLock',

    -- * Persistence
    save,
    load,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, withMVar)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as ByteString
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII')
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Page.Constants
import qualified Page.Database.Block as Block
import Page.Geometry (Direction (..), directionToOffset, opposite)
import Page.Geometry.Coordinates
import Page.QuadTree (QuadTree)
import qualified Page.QuadTree as QuadTree
import Page.Tile

-- | Incremented at each modification.
type Revision = Word64

-- | Wrapper over a 'Page.Database.Block' with revision.
data Block = Block
  { -- | Last modification revision
    bRevision :: IORef Revision,
    -- | Internal text block
    bBlock :: Block.Block
  }

-- | Create a new empty block
mkBlock :: IO Block
mkBlock = do
  bRevision <- newIORef 0
  bBlock <- Block.newBlock
  pure Block {bRevision, bBlock}

instance Show Block where
  show Block {bBlock} = show bBlock

-- | Database handle for the page
data DB = DB (IORef Revision) (MVar DB')

-- | Internal database storage type, mapping
-- coordinates to tiles.
type DB' = Map (Integer, Integer) (Tile Cell)

-- | A cell contains a quadtree of blocks and
-- represents a section of the grid.
data Cell = Cell Integer Integer (QuadTree Block)

-- | Lock type for database concurrency control
data Lock = forall a. Lock (MVar a)

getLock :: DB -> Lock
getLock (DB _ l) = Lock l

-- | Monad for executing operations with a lock
newtype Locked a = Locked (IO a)
  deriving newtype (Functor, Applicative, Monad)

-- | Execute an action with the lock held
withLock :: Lock -> Locked a -> IO a
withLock (Lock l) (Locked act) = withMVar l $ \_ -> act

-- | Execute an action with the DB lock held and potentially modify the DB'
withLock' :: DB -> (DB' -> Locked (DB', a)) -> IO a
withLock' (DB _ l) act = modifyMVar l $ \x -> case act x of
  Locked a -> a

-- | Create new empty database
new :: IO DB
new = do
  cnt <- newIORef 0
  db <- newMVar mempty
  pure $ DB cnt db

-- | Get the current global revision number
revision :: DB -> IO Word64
revision (DB ref _) = readIORef ref

-- | Convert a block to a list of text lines
blockToTexts :: Block -> [Text]
blockToTexts (Block _ cs) = go (Block.toByteString cs)
  where
    go bs
      | ByteString.null bs = []
      | otherwise = do
          let (a, bs') = ByteString.splitAt block_size bs
          let a' = case decodeASCII' a of
                Nothing -> ""
                Just x -> x
          a' : go bs'

-- | Create a new tile and insert it into the database
mkTile :: Integer -> Integer -> DB' -> IO (DB', Tile Cell)
mkTile x y db = do
  v <- newIORef undefined
  let lookupAt dir = do
        let (dx, dy) = directionToOffset dir
        case Map.lookup (x + fromIntegral dx, y + fromIntegral dy) db of
          Just ref -> do
            modifyIORef ref $
              setNeighbor (opposite dir) $
                Just v
            pure (Just ref)
          Nothing -> pure Nothing
  -- TODO: 8 bigint lookups, can be computationally expensive
  -- when current (x, y) coordinates are very very large.
  nn <- lookupAt North
  ne <- lookupAt NorthEast
  ee <- lookupAt East
  se <- lookupAt SouthEast
  ss <- lookupAt South
  sw <- lookupAt SouthWest
  ww <- lookupAt West
  nw <- lookupAt NorthWest
  let k = (x, y)
      a = Cell x y QuadTree.Empty
  writeIORef v Tile' {..}
  pure (Map.insert k v db, v)

-- | Get the tile at given position. If it doesn't
-- exist, create a new one and return it.
getTileForce :: Integer -> Integer -> DB -> IO (Tile Cell)
getTileForce x y (DB _ mvar) = do
  mem <- readMVar mvar
  case Map.lookup (x, y) mem of
    Just n -> pure n
    Nothing -> modifyMVar mvar $ mkTile x y

-- | Write a byte to a specific position in a block and update timestamps
writeAt :: DB -> Block -> BlockRelativeCharCoord -> Word8 -> IO ()
writeAt (DB cnt _) Block {bRevision, bBlock} charCoord c = do
  Block.writeAt bBlock charCoord c
  -- increase global counter
  n <- atomicModifyIORef' cnt (\n -> let m = succ n in (m, m))
  -- set block counter
  atomicWriteIORef bRevision n

-------------------------------------------------------------------------------
-- (De)Seralization

-- | Save database to a file
save :: DB -> FilePath -> IO ()
save mem fp =
  toS2 mem >>= Binary.encodeFile fp . V2

-- | Load database from a file
load :: FilePath -> IO DB
load fp =
  Binary.decodeFile fp >>= fimport

-- | Database serialization format version
data Format
  = V1 Void -- deprecated
  | V2 S2

-- | Version 2 serialization format: list of tiles with their coordinates and blocks
type S2 = [((Integer, Integer), [(QuadTree.Coord, QuadTree.Coord, Block.Block)])]

toS2 :: DB -> IO S2
toS2 (DB _ mv) = withMVar mv $ \mem -> do
  let tiles = Map.toList mem
  for tiles $ \(ij, tile) -> do
    Tile' {a = Cell _ _ qt} <- readIORef tile
    pure (ij, map getBlock $ QuadTree.toList qt)
  where
    getBlock (x, y, b) = (x, y, bBlock b)

fimport :: Format -> IO DB
fimport (V2 xs) = do
  mem <- new
  for_ xs $ \((i, j), bs) -> do
    n <- getTileForce i j mem
    t@Tile' {a = Cell _ _ qt} <- readIORef n
    let go :: [(QuadTree.Coord, QuadTree.Coord, Block.Block)] -> QuadTree Block -> IO (QuadTree Block)
        go [] !qtt = pure qtt
        go ((x, y, b) : bs') !qtt = do
          modif <- newIORef 0
          let blk = Block modif b
          go bs' $ QuadTree.insert x y blk qtt
    quadTree <- go bs qt
    writeIORef n t {a = Cell i j quadTree}
  pure mem

instance Binary.Binary Format where
  put (V2 x) = do
    Put.putByteString "PAGE"
    Put.putWord8 2
    Binary.put x
  get = do
    hasMagic <- Get.lookAhead $ do
      bs <- Get.getByteString 4
      pure (bs == "PAGE")
    version <-
      if hasMagic
        then Get.skip 4 >> Get.getWord8
        else pure 1
    case version of
      2 -> V2 <$> Binary.get
      _ -> fail $ "unknown version: " <> show version
