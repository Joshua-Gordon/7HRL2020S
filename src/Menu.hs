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

import Debug.Trace

box :: Picture
box = Color white $ Polygon [(0,0),(1000,0),(1000,50),(0,50)]

buildItemBox :: Inventory -> Recipie -> Picture
buildItemBox inv r = let i = ingredients r
                         t = Scale (0.1) (0.1) $ Text (show i ++ ": " ++ name r ++ "x" ++ show (howManny inv (name r)) ++ "    \n" ++ description r)
                     in Pictures [box, t]

arrangeItemBoxes :: [Picture] -> Int -> Picture
arrangeItemBoxes [] _ = Blank
arrangeItemBoxes (p:ps) offset = Pictures [Translate 0 (fromIntegral offset) p, arrangeItemBoxes ps (offset+50)]

displayResources :: Menu -> Menu
displayResources m = let  inv = (player_inv (player (world m)))
                          d = Pictures [
                                    Scale (0.1) (0.1) $ Text $ "Uranium: " ++ show (howManny inv "uranium"),
                                    Translate 0 (-10) $ Scale (0.1) (0.1) $ Text $ "Iron: " ++ show (howManny inv "iron"),
                                    Translate 0 (-20) $ Scale (0.1) (0.1) $ Text $ "Coal: " ++ show (howManny inv "coal"),
                                    Translate 0 (-30) $ Scale (0.1) (0.1) $ Text $ "Redstone: " ++ show (howManny inv "redstone"),
                                    Translate 0 (-40) $ Scale (0.1) (0.1) $ Text $ "Gold: " ++ show (howManny inv "gold"),
                                    Translate 0 (-50) $ Scale (0.1) (0.1) $ Text $ "Diamond: " ++ show (howManny inv "diamond"),
                                    Translate 0 (-60) $ Scale (0.1) (0.1) $ Text $ "Netherite: " ++ show (howManny inv "netherite"),
                                    Translate 0 (-70) $ Scale (0.1) (0.1) $ Text $ "Obamium: " ++ show (howManny inv "obamium")]
                       in m{resource_display=Translate 800 0 d}

initialMenu :: Map String Picture -> World -> Menu
initialMenu assets w = Menu {
        scroll_pos = -50,
        item_boxes = Translate (-300) (-590) $ arrangeItemBoxes (fmap (buildItemBox (player_inv . player $ w)) recipes) 0,
        background = case assets !? "menu.png.bmp" of
            Just m -> m
            Nothing -> error "Could not load menu image",
        world = w,
        is_paused = False,
        resource_display = Blank,
        debug_mouse = Color green $ Circle 10,
        just_crafted = False
        }

renderMenu :: Menu -> Picture
renderMenu m = if is_paused m then Pictures [background m, Translate 0 (fromIntegral (scroll_pos m)) $ item_boxes m, resource_display m, debug_mouse m] else renderWorld (world m) 
    
handleMenuEvent :: Event -> Menu -> Menu
handleMenuEvent (EventKey (SpecialKey KeyEsc) Down _ _) m = m{is_paused=False}
handleMenuEvent (EventKey (Char 'p') _ _ _) m = m{is_paused=True}
handleMenuEvent (EventKey (Char 'o') _ _ _) m = m{is_paused=False}
handleMenuEvent e m| not $ is_paused m = m{world=handleEvent e (world m)}
handleMenuEvent (EventKey (MouseButton WheelUp) _ _ _) m = m{scroll_pos= (scroll_pos m - 45)}
handleMenuEvent (EventKey (MouseButton WheelDown) _ _ _) m = m{scroll_pos=(scroll_pos m + 45)}
handleMenuEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) m = handleClick x y m
handleMenuEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) m = m{just_crafted=False}
handleMenuEvent _ m = m

updateMenu :: Float -> Menu -> Menu
updateMenu f m = displayResources m{world=tickWorld f (world m),item_boxes=Translate (-300) (-400) $ arrangeItemBoxes (fmap (buildItemBox (player_inv . player $ (world m))) recipes) 0}

handleClick :: Float -> Float -> Menu -> Menu
handleClick x y m = traceShow (x,y) $ if x > -300 && x < 700 && not (just_crafted m )
                        then let recipeIndex = fromIntegral . toInteger . round $ (400 + y - fromIntegral (scroll_pos m)) / 50
                             in if recipeIndex < (fromIntegral $ length recipes) && recipeIndex >= 0
                                then let w = world m
                                         p = player w
                                         p' = craft (recipes !! recipeIndex) p
                                     in traceShow recipeIndex m{world=w{player=p'}, debug_mouse = Translate x y $ Color green $ Circle 10, just_crafted = True}
                                else m{debug_mouse = Translate x y $ Color green $ Circle 10}
                        else m
