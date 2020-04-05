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

pickup :: String -> Int -> Inventory -> Inventory
pickup name amount inv = M.insert name (howManny inv name + amount) inv

recipes :: [Recipie]
recipes = [
    ([("uranium",1)],"fuel cell"),
    ([("iron",1),("redstone",1)],"entropy mitigator"),
    ([("gold",1),("uranium",1)],"endothermic resonator"),
    ([("iron",1),("coal",2)],"steel")
]
