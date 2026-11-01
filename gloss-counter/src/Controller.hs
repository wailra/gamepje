-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Ship

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate {
ship = shipstep (ship gstate)
}

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
input _ gstate = return gstate


