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
import Menu

main :: IO ()
main = do
      loadedAssets <- loadAssets
      let w=new_world{assets=loadedAssets}
      let m = initialMenu loadedAssets w
      playIO FullScreen black 30 m 
                              (return . renderMenu)
                        (\event world -> return $ handleMenuEvent event world)
                        (\time  world -> return $ updateMenu     time  world)
      --play FullScreen black 30 w renderWorld handleEvent tickWorld




