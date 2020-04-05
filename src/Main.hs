module Main where
import Types
import Player
import World
import Inventory
import Render
import Tick
import Graphics.Gloss.Interface.Pure.Game
import Events

renderer=undefined

main :: IO ()
main = play FullScreen black 30 new_world renderer handleEvent tickWorld
