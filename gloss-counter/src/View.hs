-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Ship

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate  = polygon (shiptopoints s) ++ 
  where
    s = ship gstate