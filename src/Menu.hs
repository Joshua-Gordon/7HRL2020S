module Menu where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Data.Map

import Types
import Player
import Inventory
import Render
import Events
import Tick

import Loader

box :: Picture
box = Polygon [(0,0),(800,0),(800,150),(0,150)]

buildItemBox :: Recipie -> Picture
buildItemBox r = let i = ingredients r
                     t = Scale (0.1) (0.1) $ Text (show i ++ ": " ++ name r ++ "\n" ++ description r)
                 in Pictures [box,Color red t]

arrangeItemBoxes :: [Picture] -> Int -> Picture
arrangeItemBoxes [] _ = Blank
arrangeItemBoxes (p:ps) offset = Pictures [Translate 0 (fromIntegral offset) p, arrangeItemBoxes ps (offset+150)]

initialMenu :: Map String Picture -> World -> Menu
initialMenu assets w = Menu {
        scroll_pos = 0,
        item_boxes = Translate 300 0 $ arrangeItemBoxes (fmap buildItemBox recipes) 0,
        background = case assets !? "menu.png.bmp" of
            Just m -> m
            Nothing -> error "Could not load menu image",
        world = w,
        is_paused = False
        }

renderMenu :: Menu -> Picture
renderMenu m = if is_paused m then Pictures [background m, Translate 0 (fromIntegral (scroll_pos m)) $ item_boxes m] else renderWorld (world m) 
    
handleMenuEvent :: Event -> Menu -> Menu
handleMenuEvent (EventKey (Char 'p') _ _ _) m = m{is_paused=True}
handleMenuEvent (EventKey (Char 'o') _ _ _) m = m{is_paused=False}
handleMenuEvent e m| not $ is_paused m = m{world=handleEvent e (world m)}
handleMenuEvent (EventKey (MouseButton WheelUp) _ _ _) m = m{scroll_pos=max 0 (scroll_pos m - 45)}
handleMenuEvent (EventKey (MouseButton WheelDown) _ _ _) m = m{scroll_pos=(scroll_pos m + 45)}
handleMenuEvent _ m = m

updateMenu :: Float -> Menu -> Menu
updateMenu f m = m{world=tickWorld f (world m)}
