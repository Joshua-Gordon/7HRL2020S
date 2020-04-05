{-# LANGUAGE MultiWayIf #-}
module Tick where

import Types
import GridManage
import Control.Monad.State
import Player
import Inventory
import Debug.Trace

mine :: World -> World
mine w =
    let
      p = player w
      px = player_x p
      py = player_y p
      targ@(px',py') = if | up w -> (px,py+1)
                          | left w -> (px-1,py)
                          | down w -> (px,py-1)
                          | right w -> (px+1,py)
                          | otherwise -> error "is mining borked"
      targetBlock :: Tile
      (targetBlock,wm') = runState (querry px' py') (worldMap w)
      p' = case targetBlock of
        Stone hardness ->  mineHardness hardness p
        Ore name hardness -> pickup name 1 (mineHardness hardness p)
        Empty -> p
      in w{
         player=p'{player_x = px',player_y = py'}
        ,worldMap=execState (setTile px' py' Empty) wm'
          }

mineHardness :: Int -> Player -> Player
isMining :: World -> Bool
isMining w = or [up w,down w,left w,right w]

urDed :: World -> Bool
urDed w = let
  p = player w
  hp = player_hull p
  pow = player_power p
  in hp < 0 || pow < 0

tickWorld :: Float -> World -> World
tickWorld t w = let w' = if | urDed w -> error "Ur ded lol (get good)"
                            | not (isMining w) -> w
                            | progress w > 0.5 ->  (mine w){progress=0}
                            | otherwise ->  w{progress=progress w + t}
                in w'{player = tick_heat . heat_damage $ player w'}
