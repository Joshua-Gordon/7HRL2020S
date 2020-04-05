module Events where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types
import Player
import Tick

handleEvent :: Event -> World -> World
handleEvent (EventKey key ks' modifiers _) w = let ks = (ks' == Down)
                                               in case key of
                                                 Char 'w' -> w{up=ks}
                                                 Char 'a' -> w{left=ks}
                                                 Char 's' -> w{down=ks}
                                                 Char 'd' -> w{right=ks}
                                                 (SpecialKey KeySpace) -> if ks then doHeat w else w
                                                 _ -> w
handleEvent _ w = w


