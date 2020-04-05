module Main where
import Types
import Player
import World
import Inventory
import Render
import Tick
import Graphics.Gloss.Interface.Pure.Game
import Events
import Loader

main :: IO ()
main = do
      loadedAssets <- loadAssets
      let w=new_world{assets=loadedAssets}
      play FullScreen black 30 w renderWorld handleEvent tickWorld



