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
step secs gstate = return gstate
 where
  s = ship gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (KeyUp Up) gstate = gstate {ship = s {upkey = False} }
input (KeyUp Down) gstate = gstate {ship = s {upkey = True} }
input (KeyLeft Up) gstate = gstate {ship = s {leftkey = False}}
input (KeyLeft Down) gstate = gstate {ship = s {leftkey = True}} 
input (KeyRight Up) gstate = gstate {ship = s {rightkey = False}}
input (KeyRight Down) gstate = gstate {ship = s {rightkey = True}}
 where
  s = ship gstate

