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
step secs gstate {}


-- | Handle user input
input :: Event -> GameState -> IO GameState
input KeyUp key gstate = gstate {keyup = key} 
input KeyLeft key gstate = gstate {keyup = key} 
input KeyRight key gstate = gstate {keyup = key} 



inputKey :: Event -> GameState -> GameState
inputKey (EventKey ("f") _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
    inputKey (EventKey (Char a) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same`