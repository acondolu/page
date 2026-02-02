module Page.Database.Cursor
  ( Cursor,
    origin,
    jumpTo,
    writeCharAt,
    writeStringAt,
    writeStrings,
    query,
    revision,
    cRevision,
  )
where

import Control.Monad (when)
import Data.ByteString.Internal (c2w)
import Data.Function ((&))
import Data.IORef (atomicWriteIORef, readIORef)
import Data.Word (Word16, Word8)
import Page.Constants
import Page.Database
import Page.Geometry
  ( Area,
    Direction (..),
    Pinned (..),
    Point (..),
    directionToOffset,
    nullA,
    offsetToDirection,
    relativeA,
    unrelative,
  )
import Page.Geometry.Coordinates
import qualified Page.QuadTree as QuadTree
import Page.Tile
  ( Tile,
    Tile' (..),
    getNeighbor,
    setNeighbor,
  )

-- | Database cursor pointing to a specific tile in the grid.
-- Can be queried efficiently.
data Cursor = Cursor
  { -- | The database
    cDB :: DB,
    -- | The current active tile
    cTile :: Tile Cell
  }

-- | Get the current global revision number.
cRevision :: Cursor -> IO Revision
cRevision Cursor {cDB} = revision cDB

-- | Make cursor pointing to the origin (0, 0)
origin :: DB -> IO Cursor
origin mm =
  Cursor mm <$> getTileForce 0 0 mm

-- | Get cursor at given absolute coordinates.
jumpTo :: AbsoluteTileCoord -> Cursor -> IO Cursor
jumpTo (AbsoluteTileCoord x y) Cursor {cDB} =
  Cursor cDB <$> getTileForce x y cDB

queryN :: Tile Cell -> Area Word16 -> IO [Pinned Word16 Block]
queryN n area
  | nullA area = pure []
  | otherwise = do
      Cell _ _ qt <- a <$> readIORef n
      pure $ QuadTree.query qt area

-- | Area coordinates are signed integers and are relative
-- to the cursor. The coordinates of the regions in output
-- should also be relative to cursor.
query :: Cursor -> Area Int -> IO [Pinned Int Block]
query Cursor {..} area
  | nullA area = pure []
  | otherwise = do
      tile <- readIORef cTile
      let resCenter = do
            let Cell _ _ qt = a tile
            area
              & relativeA (Point 0 0)
              & QuadTree.query qt
              & map (unrelative $ Point 0 0)
      let queryAtDir dir =
            case getNeighbor dir tile of
              Nothing -> pure []
              Just neighbor -> do
                -- convert direction to offset point
                let (dx, dy) = directionToOffset dir
                    offset = Point (dx * quad_size) (dy * quad_size)
                area
                  & relativeA offset
                  & queryN neighbor
                  & fmap (map (unrelative offset))
      resDirs <- mapM queryAtDir directions
      pure $ resCenter <> concat resDirs
  where
    directions =
      [ North,
        NorthEast,
        East,
        SouthEast,
        South,
        SouthWest,
        West,
        NorthWest
      ]

-- | Get block at given quad-coordinates.
-- Create if it doens't exist.
getBlockForce :: Lock -> QuadRelativeBlockCoord -> Tile Cell -> IO Block
getBlockForce l (QuadRelativeBlockCoord x y) tile = do
  -- first try without lock
  Cell _ _ qt <- a <$> readIORef tile
  case QuadTree.lookup x y qt of
    Just b -> pure b
    Nothing -> withLock l $ do
      -- retry with lock now
      cc <- Locked $ readIORef tile
      let Cell i j qt' = a cc
      case QuadTree.lookup x y qt' of
        Just b -> pure b
        Nothing -> Locked $ do
          b <- mkBlock
          let !qt'' = QuadTree.insert x y b qt'
          atomicWriteIORef tile cc {a = Cell i j qt''}
          pure b

-- | Move to an adjacent tile in the specified direction.
-- If the tile does not exist yet, create it.
moveAdjacent :: DB -> Tile Cell -> Direction -> IO (Tile Cell)
moveAdjacent db n dir = do
  n' <- readIORef n
  case getNeighbor dir n' of
    Just tile -> pure tile
    Nothing -> withLock' db $ \db' -> do
      n'' <- Locked $ readIORef n
      let Cell i j _ = a n''
      case getNeighbor dir n'' of
        Just tile -> pure (db', tile)
        Nothing -> do
          let (dx, dy) = directionToOffset dir
          (db'', tile) <- mkTile (i + dx) (j + dy) db'
          Locked $ atomicWriteIORef n $ setNeighbor dir (Just tile) n''
          pure (db'', tile)

findBlock :: Cursor -> TileRelativeTileCoord -> QuadRelativeBlockCoord -> IO Block
findBlock (Cursor db tile) (TileRelativeTileCoord xq yq) blockC = do
  tile' <-
    if xq == 0 && yq == 0
      then pure tile
      else case offsetToDirection xq yq of
        Just dir -> moveAdjacent db tile dir
        Nothing -> error $ "Invalid move direction: " ++ show (xq, yq)
  let lock = getLock db
  getBlockForce lock blockC tile'

-- | Write character at given coordinate relative
-- to the cursor.
write :: Cursor -> TileRelativeCharCoord' -> Word8 -> IO ()
write cursor coord c = do
  let db = cDB cursor
      (tileC, blockC, charC) = tileRelativeCharCoord' coord
  -- find the correct tile
  block <- findBlock cursor tileC blockC
  -- write c in block
  writeAt db block charC c

writeCharAt :: Cursor -> TileRelativeCharCoord' -> Char -> IO ()
writeCharAt cursor coord chr = do
  let w = c2w chr
  when (w /= 0) $
    write cursor coord w

writeStringAt :: Cursor -> TileRelativeCharCoord' -> String -> IO ()
writeStringAt _ _ [] = pure ()
writeStringAt cur (TileRelativeCharCoord' x y) (c : cs) = do
  writeCharAt cur (TileRelativeCharCoord' x y) c
  writeStringAt cur (TileRelativeCharCoord' (x + 1) y) cs

writeStrings :: Cursor -> TileRelativeCharCoord' -> [String] -> IO ()
writeStrings _ _ [] = pure ()
writeStrings cur (TileRelativeCharCoord' i j) (l : ls) = do
  writeStringAt cur (TileRelativeCharCoord' i j) l
  writeStrings cur (TileRelativeCharCoord' i (j + 1)) ls
