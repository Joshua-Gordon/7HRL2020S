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
  loadedAssets <- loadAssets
  w <- new_world
  let w=w{assets=loadedAssets}
  playIO FullScreen black 30 new_world renderWorld handleEvent tickWorld

