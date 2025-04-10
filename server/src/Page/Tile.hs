module Page.Tile
  ( Tile,
    Tile' (..),
    getNeighbor,
    setNeighbor,
  )
where

import Data.IORef (IORef)
import Page.Geometry (Direction (..))

type Tile a = IORef (Tile' a)

-- | A tile on the plane with neighbor connections
-- in all eight directions.
data Tile' a = Tile'
  { -- | North
    nn :: Maybe (Tile a),
    -- | NorthEast
    ne :: Maybe (Tile a),
    -- | East
    ee :: Maybe (Tile a),
    -- | SouthEast
    se :: Maybe (Tile a),
    -- | South
    ss :: Maybe (Tile a),
    -- | SouthWest
    sw :: Maybe (Tile a),
    -- | West
    ww :: Maybe (Tile a),
    -- | NorthWest
    nw :: Maybe (Tile a),
    -- | The content of the tile
    a :: a
  }

-- | Get neighbor in a specific direction
getNeighbor :: Direction -> Tile' a -> Maybe (Tile a)
getNeighbor North = nn
getNeighbor NorthEast = ne
getNeighbor East = ee
getNeighbor SouthEast = se
getNeighbor South = ss
getNeighbor SouthWest = sw
getNeighbor West = ww
getNeighbor NorthWest = nw

-- | Set neighbor in a specific direction
setNeighbor :: Direction -> Maybe (Tile a) -> Tile' a -> Tile' a
setNeighbor North t cur = cur {nn = t}
setNeighbor NorthEast t cur = cur {ne = t}
setNeighbor East t cur = cur {ee = t}
setNeighbor SouthEast t cur = cur {se = t}
setNeighbor South t cur = cur {ss = t}
setNeighbor SouthWest t cur = cur {sw = t}
setNeighbor West t cur = cur {ww = t}
setNeighbor NorthWest t cur = cur {nw = t}
