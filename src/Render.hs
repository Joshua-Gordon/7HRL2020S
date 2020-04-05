module Render where
import GridManage
import Types
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Maybe
import World
import Control.Monad.State.Lazy
import qualified Data.Map as M

renderSquare :: Assets -> Tile -> Picture
renderSquare assets sq = let
  baseImage = fromJust $ M.lookup (nameGetter sq) assets
  in case sq of
    Stone h -> Pictures [baseImage,color (makeColor 0 0 0 (0.2 * fromIntegral h/100)) (rectangleSolid 32 32)]
    Ore _ h -> Pictures [baseImage,color (makeColor 0 0 0 (0.2 * fromIntegral h/100)) (rectangleSolid 32 32)]
    _ -> baseImage

number :: [[a]] -> [[((Int,Int),a)]]
number = map (map (\(y,(x,z)) -> ((x,y),z))) . zipWith (zip . repeat) [0..] . map (zip [0..])

drawAtPos ::  (Int,Int) -> Picture -> Picture
drawAtPos (x,y) = let
  x' = 32 * (fromIntegral x - 30)
  y' = 32 * (fromIntegral y - 30)
  in translate x' y'

nameGetter :: Tile -> String
nameGetter (Stone _) = "stone.png.bmp"
nameGetter (Ore o _) = o ++ ".png.bmp"
nameGetter Empty = "empty.png.bmp"

renderGrid :: Int -> Int -> Assets -> GridState Picture 
renderGrid x y assets = do
  squares <- getGrid generator (x-30) (y-30) (x+30) (y+30)
  let numbered = number squares :: [[((Int,Int),Tile)]]
  let pics = map (map (\(p,s) -> drawAtPos p (renderSquare assets s))) numbered
  return $ Pictures (concat pics)

renderWorld :: World -> Picture
renderWorld w = let p = player w
                    x = player_x p
                    y = player_y p
                    pic = evalState (renderGrid x y (assets w)) (worldMap w)
                    playerPic = fromJust $ M.lookup "player.png.bmp" (assets w)
                in Pictures [pic,playerPic]

