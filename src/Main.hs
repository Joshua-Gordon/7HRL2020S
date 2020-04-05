module Main where
import Types
import Player
import World
import Inventory
import Render
import Tick
import Graphics.Gloss.Interface.IO.Game
import Events
import Loader

main :: IO ()
main = do
  w <- new_world
  playIO FullScreen black 30 w renderWorld handleEvent tickWorld

