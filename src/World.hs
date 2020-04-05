module World where

import Types
import qualified Data.Map as M
import Numeric.Noise.Perlin
import Data.Bits
import System.Random
import Player

import System.IO.Unsafe

make_noise :: Int -> Perlin
make_noise s = let seed = s
                   octaves = 6
                   scale = 0.05
                   persistence = 0.6
               in perlin seed octaves scale persistence

world_noise = make_noise (unsafePerformIO randomIO)
ore_noise   = make_noise (unsafePerformIO randomIO)

new_world :: World
new_world = World {
     worldMap=M.empty
    ,player=new_player
    ,up=False
    ,down=False
    ,left=False
    ,right=False
    ,progress=0
    ,assets=M.empty
    ,venting=False
}

generate_tile :: (Int,Int) -> Tile
generate_tile (x,y) = let noise = noiseValue world_noise (fromIntegral x+10000,fromIntegral y+10000,0)
                          hardness = min 100 (100 * (noise + 1)/2 + 0.0005*(fromIntegral y))
                          noise' = noiseValue ore_noise (fromIntegral x, fromIntegral y,0)
                          ore = min 100 (100 * (noise' + 1)/2)
                          int_hardness = fromIntegral . toInteger . round $ hardness
                      in if hardness >= 80 && ore >= 60
                        then let seed = (x `div` 8) `xor` (y `div` 8)
                                 gen = mkStdGen seed
                                 val = head $ randoms gen :: Double
                             in if val <= 0.35
                                       then Ore "uranium" int_hardness
                                       else if val <= 0.65
                                       then Ore "iron" int_hardness
                                       else if val <= 0.75
                                       then Ore "coal" int_hardness
                                       else if val <= 0.85
                                       then Ore "redstone" int_hardness
                                       else if val <= 0.925
                                       then Ore "gold" int_hardness
                                       else if val <= 0.95
                                       then Ore "diamond" int_hardness
                                       else if val <= 0.975
                                       then Ore "netherite" int_hardness
                                       else Ore "obamium" int_hardness
                        else Stone int_hardness 
