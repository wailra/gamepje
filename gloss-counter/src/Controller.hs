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
import Bullet

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step dt gstate = do

    let newTimer = spawnTimer gstate + dt

    if newTimer >= 10
        then do
            asteroid <- spawnasteroid

            return (collisioncheckship (collisioncheckbullet (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = asteroid : map asteroidstep (asteroids gstate)
                , spawnTimer = 0
                })))

        else
            return (collisioncheckship (collisioncheckbullet (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = map asteroidstep (asteroids gstate)
                , spawnTimer = newTimer
                })))

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

-- Bullets which touch an asteroid disappear and asteroids which get touched by a bullet disappear too
collisioncheckbullet :: GameState -> GameState
collisioncheckbullet gstate = gstate
    { ship      = s { bullets = filter (not . bullethit) bs }
        , asteroids = filter (not . asteroidhit) as ++ concatMap splitasteroid hitasteroids
    }
  where
    s  = ship gstate
    bs = bullets s
    as = asteroids gstate
    -- bullet gets used up when it touches any asteroid
    bullethit b   = touchesany (map abody as) (bbody b)
    -- asteroid destroyed when any bullet touches it
    asteroidhit a = touchesany (map bbody bs) (abody a)
    -- all asteroids which got hit in thisssssss frame
    hitasteroids  = filter asteroidhit as