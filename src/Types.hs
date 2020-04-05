module Types where
import Control.Monad.State
import qualified Data.Map as M

data Tile = Stone Int | Ore String Int

type TileMap = M.Map (Int,Int) Tile

type GridState a = State TileMap a

data World = World {
    worldMap :: TileMap
}
type Inventory = M.Map String Int

data Player = Player {
    player_x :: Int,
    player_y :: Int,
    player_hull :: Int,
    player_heat :: Int,
    heat_thresh :: Int,
    player_inv :: Inventory,
    radiator :: Int,
    inv_cap :: Int,
    has_smelt :: Bool
}

