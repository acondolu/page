{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Database.Utils
  ( statistics,
  )
where

import Control.Concurrent.MVar (withMVar)
import Data.IORef (readIORef)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as Map
import Page.Database
import Data.Traversable (for)
import Page.Database.Block (bempty, bcount)
import Page.Tile (Tile' (..))
import qualified Page.QuadTree as QuadTree
import System.Log.FastLogger (ToLogStr (..))
import GHC.Generics (Generic)

data Statistics = Statistics
  { nTiles :: Int, -- ^ Number of tiles
    nBlocks :: Int, -- ^ Number of blocks
    nEmptyTiles :: Int, -- ^ Number of empty tiles
    nEmptyBlocks :: Int, -- ^ Number of empty blocks
    n1EmptyBlocks :: Int -- ^ Number of blocks with only one non-empty character
  }
  deriving (Generic, ToJSON)

statistics :: DB -> IO Statistics
statistics (DB _ mv) = withMVar mv $ \mem -> do
  let tiles = Map.toList mem
  tilesStas <- for tiles $ \((i, j), tile) -> do
    Tile' {a = Cell _ _ qt} <- readIORef tile
    let nblocks = length $ QuadTree.toList qt
        emptyBlocks = length $ filter (\(_, _, b) -> bempty $ bBlock b) $ QuadTree.toList qt
        blocks1 = length $ filter (\(_, _, b) -> bcount (bBlock b) == 1) $ QuadTree.toList qt
    pure ((i, j), nblocks, emptyBlocks, blocks1)
  pure Statistics
    { nTiles = length tiles,
      nBlocks = sum [n | (_, n, _, _) <- tilesStas],
      nEmptyBlocks = sum [n | (_, _, n, _) <- tilesStas],
      n1EmptyBlocks = sum [n | (_, _, _, n) <- tilesStas],
      nEmptyTiles = length [() | (_, n, m, _) <- tilesStas, n == m]
    }

instance ToLogStr Statistics where
  toLogStr = toLogStr . encodePretty
