module Player where

import Types
import Data.Maybe
import Data.Map

new_player :: Player
new_player = Player {
    player_x = 0,
    player_y = 0,
    player_hull = 100,
    player_hull_max = 100,
    player_power = 100,
    player_power_max = 100,
    player_heat = 0,
    heat_thresh = 20,
    player_inv = fromList [
        ("uranium",0),
        ("iron",0),
        ("coal",0),
        ("redstone",0),
        ("gold",0),
        ("diamond",0),
        ("netherite",0),
        ("obamium",0),
        ("steel",0),
        ("depleted uranium",0)
    ],
    radiator = 1,
    inv_cap = 40,
    has_smelt = False
}

tick_heat :: Player -> Player
tick_heat p = let u = fromJust $ (player_inv p) !? "uranium"
              in p{player_heat=player_heat p + u `div` 2}

heat_damage :: Player -> Player
heat_damage p = let heat_diff = (player_heat p) - (heat_thresh p)
                in if heat_diff > 0 then p{player_hull=(player_hull p) - heat_diff,player_heat=player_heat p - (heat_diff `div` 2)} else p
