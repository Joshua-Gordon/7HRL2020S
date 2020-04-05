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

craft :: Recipie -> Inventory -> (Bool,Inventory)
craft (comp,makes) inv = case useStuff comp inv of
  Just inv' -> (True,M.insert makes 1 inv')
  Nothing -> (False,inv)

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

recipes :: [Recipie]
recipes = [
    ([("uranium",1)],"fuel cell"),
    ([("iron",1),("redstone",1)],"entropy mitigator"),
    ([("gold",1),("uranium",1)],"endothermic resonator"),
    ([("iron",1),("coal",2)],"steel"),
    ([("uranium",5),("redstone",5),("steel",1)],"depleted uranium"),
    ([("obamium",30)],"obamids"),
    ([("steel",10),("redstone",2)],"steel drill"),
    ([("steel",15)],"steel hull"),
    ([("depleted uranium",10),("redstone",2)],"depleted uranium drill"),
    ([("depleted uranium",15)],"depleted uranium hull"),
    ([("diamond",10),("redstone",2)],"diamond drill")]
