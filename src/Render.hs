module Render where
import GridManage
import Types
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Maybe
import World
import Control.Monad.State.Lazy
import Text.Printf
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

-- copy paste lmao
arrangeBoxes :: [Picture] -> Float -> Picture
arrangeBoxes [] _ = Blank
arrangeBoxes (p:ps) offset = Pictures [translate 0 offset p, arrangeBoxes ps (offset-150)]

infoPanel :: Assets -> Player -> Picture
infoPanel assets p = 
  -- hope this rectangle is large enough lmao
  let menu_bg = color black $ rectangleSolid 370 500
      hull = text (printf "Hull: %d/%d" (player_hull p) (player_hull_max p))
      power = text (printf "Power: %d/%d" (player_power p) (player_power_max p))
      heat = text (printf "Heat: %d/%d" (player_heat p) (heat_thresh p))
      endothermic = if player_heat_immune p > 0 then text "Resonator Running"
                       else text "Resonator Missing"
      radiator' = text (printf "Radiator Level: %d" (Types.radiator p))
      smelter = text (if has_smelt p then "Smelter" else "Smeltern't")
      drill = text (printf "Drill: %d" (drill_level p))
  in
    translate 640 900 $ Pictures [translate 150 (-100) menu_bg, color white $ scale 0.3 0.3 (arrangeBoxes [hull, power, heat, endothermic, radiator', smelter, drill] 0)]

renderWorld :: World -> Picture
renderWorld w = let p = player w
                    x = player_x p
                    y = player_y p
                    pic = evalState (renderGrid x y (assets w)) (worldMap w)
                    playerPic = fromJust $ M.lookup "player.png.bmp" (assets w)
                in Pictures [pic,playerPic, infoPanel (assets w) (player w)]

