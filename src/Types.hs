module Types where
import Control.Monad.State
import qualified Data.Map as M

data Tile = Stone Int | Ore String Int

type TileMap = M.Map (Int,Int) Tile

type GridState a = State TileMap a

type Inventory = M.Map String Int
type Components = [(String,Int)]
type Recipie = (Components,String)

data World = World {
     worldMap :: TileMap
    ,player :: Player
}

data Player = Player {
    player_x :: Int,
    player_y :: Int,
    player_hull_max :: Int,
    player_hull :: Int,
    player_power :: Int,
    player_power_max :: Int,
    player_heat :: Int,
    heat_thresh :: Int,
    player_heat_immune :: Int,
    player_inv :: Inventory,
    radiator :: Int,
    inv_cap :: Int,
    has_smelt :: Bool,
    drill_level :: Int
}

