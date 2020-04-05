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
import Menu

main :: IO ()
main = do
      loadedAssets <- loadAssets
      let w=new_world{assets=loadedAssets}
      let m = initialMenu loadedAssets w
      play FullScreen black 30 m renderMenu handleMenuEvent updateMenu
      --play FullScreen black 30 w renderWorld handleEvent tickWorld





