module Types where
import Control.Monad.State
import qualified Data.Map as M

data Tile = Stone Int | Ore String Int

type GridState square a = State (M.Map (Int,Int) square) a

data World = World {
    worldMap :: GridState Tile [[Tile]]
}
