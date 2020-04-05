module Render where
import GridManage
import Types
import Graphics.Gloss
import Data.Maybe
import World
import Control.Monad.State.Lazy
import Debug.Trace
import qualified Data.Map as M

renderSquare :: Assets -> Tile -> Picture
renderSquare assets sq = fromJust $ M.lookup (nameGetter sq) assets

number :: [[a]] -> [[((Int,Int),a)]]
number = map (map (\(y,(x,z)) -> ((x,y),z))) . zipWith (zip . repeat) [0..] . map (zip [0..])

drawAtPos ::  (Int,Int) -> Picture -> Picture
drawAtPos (x,y) = let
  x' = 32 * (fromIntegral x)
  y' = 32 * (fromIntegral y)
  in traceShow (x',y') $ translate x' y'

nameGetter :: Tile -> String
nameGetter (Stone _) = "stone.png.bmp"
nameGetter (Ore o _) = o ++ ".png.bmp"

renderGrid :: Int -> Int -> Assets -> GridState Picture 
renderGrid x y assets = do
  squares <- getGrid generator (x-100) (y-100) (x+100) (y+100)
  let numbered = number squares :: [[((Int,Int),Tile)]]
  let pics = map (map (\(p,s) -> drawAtPos p (renderSquare assets s))) numbered
  return $ Pictures (concat pics)

renderWorld :: World -> Picture
renderWorld w = let p = player w
                    x = player_x p
                    y = player_y p
                    pic = evalState (renderGrid x y (assets w)) (worldMap w)
                in pic

