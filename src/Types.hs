module Types where
import Control.Monad.State
import qualified Data.Map as M
import Graphics.Gloss

data Tile = Stone Int | Ore String Int | Empty deriving Show

type TileMap = M.Map (Int,Int) Tile

type GridState a = State TileMap a

type Inventory = M.Map String Int
type Components = [(String,Int)]

data Recipie = Recipie {
    ingredients :: Components
   ,name :: String
   ,description :: String
}

type Assets = M.Map String Picture

data World = World {
     worldMap :: TileMap
    ,player :: Player
    ,up :: Bool
    ,down :: Bool
    ,left :: Bool
    ,right :: Bool
    ,venting :: Bool
    ,progress :: Float
    ,assets :: Assets
}deriving Show

data Task = Mining | Crawling | Venting

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
} deriving Show

data Menu  = Menu {
    scroll_pos :: Int,
    background :: Picture,
    item_boxes :: Picture,
    resource_display :: Picture,
    world :: World,
    is_paused :: Bool,
    debug_mouse :: Picture,
    just_crafted :: Bool
}
