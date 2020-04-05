{-# LANGUAGE MultiWayIf #-}
module Tick where

import Types
import GridManage

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
      targetBlock :: Tile
      targetBlock = querry  px' py'
      p' = case targetBlock of
        Stone hardness ->  mineHardness hardness p
        Ore name hardness -> mineHardness hardness p
      in undefined

mineHardness :: Int -> Player -> Player
mineHardness hardness p = p{player_heat=player_heat p + (hardness - drill_level p) / 5}


tickWorld :: Float -> World -> World
tickWorld t w 
  | progress > 0.5 = mine w
  | otherwise = w{progress=progress w + t}

