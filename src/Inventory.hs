module Inventory where

import qualified Data.Map as M
import Control.Applicative
import Types
import Data.Maybe

howManny :: Inventory -> String -> Int
howManny inv name = fromJust $ M.lookup name inv <|> Just 0

useStuff :: Components -> Inventory -> Maybe Inventory
useStuff [] i = Just i
useStuff ((name,needed):xs) inv
  | have < needed = Nothing
  | otherwise = Just $ M.insert name (have - needed) inv
    where
      have = howManny inv name

craft :: Recipie -> Inventory -> Inventory
craft (comp,makes) inv = case useStuff comp inv of
  Just inv' -> M.insert makes 1 inv' 
  Nothing -> inv

pickup :: String -> Int -> Player -> Player
pickup name amount p =
  let 
    inv = player_inv p
    cap = inv_cap p
    used = spaceUsed inv
    spaceLeft = cap - used
    amount' = min spaceLeft amount
  in p{player_inv=M.insert name (howManny inv name + amount') inv}

has :: String -> Inventory -> Bool
has name inv = isJust $ M.lookup name inv

spaceUsed :: Inventory -> Int
spaceUsed inv = sum $ map snd (M.toList inv)
