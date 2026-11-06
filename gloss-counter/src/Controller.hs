-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Ship
import Asteroid
import Body

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step dt gstate = do

    let newTimer = spawnTimer gstate + dt

    if newTimer >= 10
        then do
            asteroid <- spawnasteroid

            return (collisioncheckship (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = asteroid : map asteroidstep (asteroids gstate)
                , spawnTimer = 0
                }))

        else
            return (collisioncheckship (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = map asteroidstep (asteroids gstate)
                , spawnTimer = newTimer
                }))

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyUp) Down _ _) gstate =
    return gstate {ship = (ship gstate) {upkey = True}}
input (EventKey (SpecialKey KeyUp) Up _ _) gstate =
    return gstate {ship = (ship gstate) {upkey = False}}
input (EventKey (SpecialKey KeyLeft) Down _ _) gstate =
    return gstate {ship = (ship gstate) {leftkey = True}}
input (EventKey (SpecialKey KeyLeft) Up _ _) gstate =
    return gstate {ship = (ship gstate) {leftkey = False}}
input (EventKey (SpecialKey KeyRight) Down _ _) gstate =
   return gstate {ship = (ship gstate) {rightkey = True}}
input (EventKey (SpecialKey KeyRight) Up _ _) gstate =
     return gstate {ship = (ship gstate) {rightkey = False}}
input (EventKey (SpecialKey KeySpace) Down _ _) gstate =
     return gstate {ship = (ship gstate) {spacebar = True}}
input (EventKey (SpecialKey KeySpace) Up _ _) gstate =
     return gstate {ship = (ship gstate) {spacebar = False}}
input _ gstate = return gstate

collisioncheckship :: GameState -> GameState
collisioncheckship gstate = if check
    then gstate 
    {
        lives = (lives gstate) - 1
        ,ship = spawnShip
    }
    else gstate
        where
            check = any (bodycollisioncheck (sbody (ship gstate))) (map abody (asteroids gstate))

collisioncheckbullet :: GameState -> GameState
collisioncheckbullet g =
 where
    s = ship gstate
    b = bullets s
    a = asteroids gstate
    asteroidcheck [] = []
    asteroidcheck (x:xs) | !(any (bodycollisioncheck (bbody b))) = x : asteroidcheck xs
    asteroidcheck (x:xs) | otherwwise = asteroidcheck xs

