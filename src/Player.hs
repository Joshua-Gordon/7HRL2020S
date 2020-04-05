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
    player_heat_immune = 5,
    heat_thresh = 20,
    player_inv = fromList [
        ("uranium",0)
    ],
    radiator = 1,
    inv_cap = 40,
    has_smelt = False,
    drill_level = 20
}

tick_heat :: Player -> Player
tick_heat p = let u = fromJust $ (player_inv p) !? "uranium"
              in p{player_heat=player_heat p + u `div` 2,player_heat_immune = if player_heat_immune p == 0 then 0 else player_heat_immune p - 1}

heat_damage :: Player -> Player
heat_damage p = let heat_diff = (player_heat p) - (heat_thresh p)
                in if player_heat_immune p > 0 then p else if heat_diff > 0 then p{player_hull=(player_hull p) - heat_diff,player_heat=player_heat p - (heat_diff `div` 2)} else p
