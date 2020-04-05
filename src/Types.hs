module Types where
import Control.Monad.State
import qualified Data.Map as M

data Tile = Stone Int | Ore String Int

type GridState square a = State (M.Map (Int,Int) square) a

data World = World {
    map :: GridState Tile [[Tile]]
}
type Inventory = M.Map String Int

data Player = Player {
    player_x :: Int,
    player_y :: Int,
    player_hull :: Int,
    player_heat :: Int,
    heat_thresh :: Int,
    player_inv :: Inventory
}

