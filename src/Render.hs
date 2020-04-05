module Render where
import GridManage
import Types
import Graphics.Gloss
import Data.Maybe
import qualified Data.Map as M

generator = undefined  
  
type Assets = M.Map String Picture

renderSquare :: (Tile -> String) -> Assets -> Tile -> Picture
renderSquare nameGetter assets sq = fromJust $ M.lookup (nameGetter sq) assets

number :: [[a]] -> [[((Int,Int),a)]]
number = map (map (\(x,(y,z)) -> ((x,y),z))) . zipWith (zip . repeat) [1..] . map (zip [1..])

drawAtPos ::  (Int,Int) -> Picture -> Picture
drawAtPos (x,y) = translate (32 * fromIntegral x - 30) ((-32) * fromIntegral y - 18) 

renderGrid :: Int -> Int -> (Tile -> String) -> Assets -> GridState Picture 
renderGrid x y nameGetter assets = do
  squares <- getGrid generator (x-30) (y-18) (x+30) (y+18)
  let numbered = number squares :: [[((Int,Int),Tile)]]
  let pics = map (map (\(p,s) -> drawAtPos p (renderSquare nameGetter assets s))) numbered
  return $ Pictures (concat pics)

