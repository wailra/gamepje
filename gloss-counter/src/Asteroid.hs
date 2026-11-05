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

spawnasteroid :: IO Asteroid
spawnasteroid = do
    siz <- randomRIO (1, 4)
    b <- spawnBodyside

    let b' = b {collisionsize = fromIntegral siz * 40}

    return (Asteroid b' siz)
