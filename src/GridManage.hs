module GridManage where

import Control.Monad.State
import qualified Data.Map as M

expandState :: (b -> a) -> (a -> b -> b) -> State a c -> State b c
expandState getter setter s = do
  b <- get
  let a = getter b
  let (c,a2) = runState s a
  let b2 = setter a2 b
  put b2
  return c

type GridState square a = State (M.Map (Int,Int) square) a

querry :: (Int -> Int -> square) -> Int -> Int ->  GridState square square
querry generator x y = do
  saved <- get
  case M.lookup (x,y) saved of
    Just square -> return square
    Nothing -> let
      square = generator x y
      in modify (M.insert (x,y) square) >> return square

setSquare :: Int -> Int -> square -> GridState square ()
setSquare x y square = modify $ M.insert (x,y) square

getGrid :: (Int -> Int -> square) -> Int -> Int -> Int -> Int -> GridState square [[square]]
getGrid generateor xMin yMin xMax yMax = let
  querry' = uncurry $ querry generateor
  in (mapM.mapM)  querry' [[ (x,y) | x <- [xMin..xMax] ] | y <- [yMin..yMax] ]
 
