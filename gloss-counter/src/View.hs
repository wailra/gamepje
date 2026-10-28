-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate {Ship = ship} =
  Drawship ship {Body = body }-> polygon [(location + size, 0.0), (100.0, 0.0), (50.0, 100.0)]