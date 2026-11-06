module Asteroid where

import Body
import System.Random


data Asteroid = Asteroid 
    {
        abody :: Body
    ,   size :: Int
    }


asteroidstep :: Asteroid -> Asteroid
asteroidstep a = a {abody = boundingboxBody (moveBody (abody a))}

spawnasteroid :: StdGen -> (Asteroid, StdGen)
spawnasteroid gen =
    let
        (siz, gen1) = randomR (1, 4) gen :: (Int, StdGen)
        (b, gen2) = spawnBodyside gen1

        b' = b { collisionsize = fromIntegral siz * 40 }

    in
        (Asteroid b' siz, gen2)

-- What is being left after an asteroid is shot
-- Size 1 is gone and big ones break into two pieces of a size smaller
splitasteroid :: Asteroid -> [Asteroid]
splitasteroid a
    | size a <= 1 = []
    | otherwise   = [piece (vy, -vx), piece (-vy, vx)]
  where
    b        = abody a
    (vx, vy) = velocity b          -- old speed of the asteroid
    newsize  = size a - 1
    -- same place, new direction is a quarter turn, a little faster ands maller
    piece (x, y) = Asteroid (b { velocity = (x * 1.2, y * 1.2)
                               , collisionsize = fromIntegral newsize * 40 })
                            newsize

-- Points for shooting this asteroid and smaller is worth more. Size 4 = 10, size 3 = 20, size 2 = 30, size 1 = 40
asteroidpoints :: Asteroid -> Int
asteroidpoints a = (5 - size a) * 10