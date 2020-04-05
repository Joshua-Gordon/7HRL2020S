module Menu where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Types
import Player
import Inventory

import Loader

buildItemBox 

initialMenu :: IO Menu
initialMenu = do
    assets <- loadAssets
    return $ Menu {
        scroll_pos = 0,
        item_boxes = [],
        background = case assets ?! "menu.png.bmp" of
            Just m -> m
            Nothing -> error "Could not load menu image"
    }

--renderMenu :: Menu -> IO Picture
--renderMenu =  do
    

