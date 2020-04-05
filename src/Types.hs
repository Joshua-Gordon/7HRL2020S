module Types where
import Control.Monad.State
import qualified Data.Map as M

data Tile = Stone Int | Ore String Int

type TileMap = M.Map (Int,Int) Tile

type GridState a = State TileMap a

data World = World {
    worldMap :: TileMap
}
