module Menu where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Types
import Player
import Inventory

import Loader

box :: Picture
box = Polygon [(0,0),(800,0),(800,150),(0,150)]

buildItemBox :: Recipie -> Picture
buildItemBox r = let i = ingredients r
                     t = Text (show i ++ ": " ++ name r ++ "\n" ++ description r)
                 in Pictures [t,box]

arrangeItemBoxes :: [Picture] -> Int -> Picture
arrangeItemBoxes [] _ = Blank
arrangeItemBoxes (p:ps) offset = Pictures [Translate offset p, arrangeItemBoxes ps (offset+150)]

initialMenu :: assets -> World -> Menu
initialMenu assets w = Menu {
        scroll_pos = 0,
        item_boxes = arrangeItemBoxes (map buildItemBox recipes) 0,
        background = case assets ?! "menu.png.bmp" of
            Just m -> m
            Nothing -> error "Could not load menu image",
        world = w
        }

renderMenu :: Menu -> Picture
renderMenu m = Pictures [Translate (0,scroll_pos m) $ item_boxes m, background m]
    
handleMenuEvent :: Event -> Menu -> Menu
handleMenuEvent (EventKey (MouseButton WheelUp) _ _ _) m = m{scroll_pos=max 0 (scroll_pos m - 45)}
handleMenuEvent (EventKey (MouseButton WheelDown) _ _ _) m = m{scroll_pos=(scroll_pos m + 45)}
handleMenuEvent _ m = m

updateMenu :: Float -> Menu -> Menu
updateMenu _ m = m
