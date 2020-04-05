{-# LANGUAGE MultiWayIf #-}
module Tick where

import Types
import GridManage
import Control.Monad.State
import Inventory

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
      (targetBlock,wm') = runState (querry px' py') (worldMap w)
      p' = case targetBlock of
        Stone hardness ->  mineHardness hardness p
        Ore name hardness -> pickup name 1 (mineHardness hardness p)
      in undefined

mineHardness :: Int -> Player -> Player
mineHardness hardness p = p{player_heat=player_heat p + (hardness - drill_level p) `div` 5,player_power=player_power p - round (sqrt (fromIntegral hardness))}

tickWorld :: Float -> IO World -> IO (IO World)
tickWorld t iw = do
                  w <- iw
                  return $ if progress w > 0.5 
                      then return $ (mine w){progress=0}
                      else return $ w{progress=progress w + t}

