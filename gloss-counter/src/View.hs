-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Ship
import Asteroid
import Body
import Bullet

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures
    [ color white (polygon (shiptopoints s))
    , pictures (map drawAsteroid (asteroids gstate))
    , pictures (map drawBullet (bullets (ship gstate)))
    ]
    where
        s = ship gstate

drawAsteroid :: Asteroid -> Picture
drawAsteroid a =
    translate x y (color red (thickCircle radius thickness))
    where
        b = abody a
        (x, y) = location b
        radius = collisionsize b
        thickness = 10

drawBullet :: Bullet -> Picture
drawBullet a =
    translate x y (color white (thickCircle radius thickness))
    where
        b = bbody a
        (x, y) = location b
        radius = collisionsize b
        thickness = 3