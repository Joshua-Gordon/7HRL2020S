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
      in traceShow (player_power (player w)) w{
         player=p'{player_x = px',player_y = py'}
        ,worldMap=execState (setTile px' py' Empty) wm'
          }

mineHardness :: Int -> Player -> Player
mineHardness hardness p = p{player_heat=player_heat p + (hardness - drill_level p) `div` 5,player_power=player_power p - round (0.3 * sqrt (fromIntegral hardness))}

isMining :: World -> Bool
isMining w = or [up w,down w,left w,right w]

tickWorld :: Float -> World -> World
tickWorld t w = let w' = if | not (isMining w) -> w
                            | progress w > 0.5 ->  (mine w){progress=0}
                            | otherwise ->  w{progress=progress w + t}
                in w'{player = tick_heat . heat_damage $ player w'}
