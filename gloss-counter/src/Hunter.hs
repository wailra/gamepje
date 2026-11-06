module Hunter where

import Body
import Math

data Hunter = Hunter
    { hbody :: Body
    }

-- How fast a hunter flies (px/f)
hunterspeed :: Float
hunterspeed = 15

-- Make new hunter on a random side of the screen.
spawnhunter :: IO Hunter
spawnhunter = do
    b <- spawnBodyside
    return (Hunter (b { collisionsize = 30 }))

-- Aim the hunter at the ship and move it one frame (My brain is burning at this point) same sx sy as in the UFO
hunterstep :: (Float, Float) -> Hunter -> Hunter
hunterstep (sx, sy) h = h { hbody = moveBody (b { velocity = (vx, vy) }) }
  where
    b        = hbody h
    (hx, hy) = location b
    d        = max 1 (distancepoints (hx, hy) (sx, sy))   -- DOOOON"T EVER DIVIDE BY 0 (max 1)
    vx       = (sx - hx) / d * hunterspeed
    vy       = (sy - hy) / d * hunterspeed