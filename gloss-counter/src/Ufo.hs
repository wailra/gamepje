module Ufo where

import Body
import Bullet
import Math

-- UFO flies around the screen and shoots towards the ship
data Ufo = Ufo
    { ubody      :: Body
    , shoottimer :: Float   -- last shot when?
    }

-- Delay between the shots from a UFO
ufoshootdelay :: Float
ufoshootdelay = 2

-- How fast a UFO bullet flies (px/f)
ufobulletspeed :: Float
ufobulletspeed = 25

-- Make a new UFO on one random side of the screen
spawnufo :: IO Ufo
spawnufo = do
    b <- spawnBodyside
    return (Ufo (b { collisionsize = 35 }) 0)

-- Did UFO wait long enough to shoot again?
readytoshoot :: Ufo -> Bool
readytoshoot u = shoottimer u >= ufoshootdelay

-- Move UFO 1 frame, also wraps like an asteroid around the screen
--  update the timer for it
ufostep :: Float -> Ufo -> Ufo
ufostep dt u = u { ubody = boundingboxBody (moveBody (ubody u)), shoottimer = newtimer }
  where
    newtimer | readytoshoot u = 0
             | otherwise      = shoottimer u + dt

-- A new bullet from the UFO flies towards the ship
--   also (sx, sy) is where the ship is located at that moment :)
ufoshoot :: (Float, Float) -> Ufo -> Bullet
ufoshoot (sx, sy) u = Bullet (bulletbody { location = (ux, uy), velocity = (vx, vy) }) 60
  where
    (ux, uy) = location (ubody u)
    d        = max 1 (distancepoints (ux, uy) (sx, sy))   -- 😬😬😬 x Furkan
    vx       = (sx - ux) / d * ufobulletspeed
    vy       = (sy - uy) / d * ufobulletspeed