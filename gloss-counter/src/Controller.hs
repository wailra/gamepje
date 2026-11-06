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
import Hunter
import Ufo


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step dt gstate = do

    let newTimer = spawnTimer gstate + dt

    if newTimer >= 10
        then do
            asteroid <- spawnasteroid
            hunter   <- spawnhunter
            ufo <- spawnufo

            return (collisioncheckship (collisioncheckbullet (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = asteroid : map asteroidstep (asteroids gstate)
                , hunters = addifnone hunter movedhunters
                , ufos = addifnone ufo movedufos
                , ufobullets = movedufobullets
                , spawnTimer = 0
                })))

        else
            return (collisioncheckship (collisioncheckbullet (gstate
                { elapsedTime = elapsedTime gstate + dt
                , ship = shipstep (ship gstate)
                , asteroids = map asteroidstep (asteroids gstate)
                , hunters = movedhunters
                , ufos = movedufos
                , ufobullets = movedufobullets
                , spawnTimer = newTimer
                })))
  where
    -- where is the shippie
    shippos = location (sbody (ship gstate))
    -- hunters take one steppie towards the shippie
    movedhunters = map (hunterstep shippos) (hunters gstate)
    -- each UFO moves one frame
    movedufos = map (ufostep dt) (ufos gstate)
    -- each UFO ready, fires a new bullet towards the ship
    newshots = map (ufoshoot shippos) (filter readytoshoot (ufos gstate))
    -- old UFO bullets move and be gone when fate has been completed
    movedufobullets = newshots ++ filter lifetimecheck (map bulletstep (ufobullets gstate))
    -- only add the new enemy when there is none of that sort yet
    addifnone x [] = [x]
    addifnone _ xs = xs

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
        ,hunters = []
        ,ufobullets = [] 
    }
    else gstate
        where
            -- everything which kills the ship when it touches it
            dangers = map abody (asteroids gstate)  ++ map ubody (ufos gstate) ++ map bbody (ufobullets gstate)
            check   = touchesany dangers (sbody (ship gstate))

-- Bullets which touch an asteroid disappear and asteroids which get touched by a bullet disappear too
collisioncheckbullet :: GameState -> GameState
collisioncheckbullet gstate = gstate
    { ship      = s { bullets = filter (not . bullethit) bs }
        , asteroids = filter (not . asteroidhit) as ++ concatMap splitasteroid hitasteroids
        , hunters = filter (not . hunterhit) hs
        , ufos = filter (not . ufohit) us
        , score = score gstate + points
    }
  where
    s  = ship gstate
    bs = bullets s
    as = asteroids gstate
    hs = hunters gstate
    us = ufos gstate
    -- bullet gets used up when it touches any asteroid
    bullethit b   = touchesany (map abody as ++ map hbody hs ++ map ubody us) (bbody b)
    -- asteroid destroyed when any bullet touches it etc etc
    asteroidhit a = touchesany (map bbody bs) (abody a)
    hunterhit h   = touchesany (map bbody bs) (hbody h)
    ufohit u      = touchesany (map bbody bs) (ubody u)
    -- all asteroids which got hit in thisssssss frame
    hitasteroids  = filter asteroidhit as
    -- points earned on this frame is asteroids by size, 50 per hunter and 100 per UFO
    points = sum (map asteroidpoints hitasteroids)
           + 50  * length (filter hunterhit hs)
           + 100 * length (filter ufohit us)