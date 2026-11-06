-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Ship
import Asteroid
import Body
import Bullet
import Hunter
import Ufo

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures
    [ color white (polygon (shiptopoints s))
    , pictures (map drawAsteroid (asteroids gstate))
    , pictures (map drawBullet (bullets (ship gstate)))
    , pictures (map drawHunter (hunters gstate))
    , pictures (map drawUfo (ufos gstate))
    , pictures (map drawBullet (ufobullets gstate))
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

-- hunter is a filled orange circle
drawHunter :: Hunter -> Picture
drawHunter h = translate x y (color orange (circleSolid (collisionsize b)))
    where
        b      = hbody h
        (x, y) = location b

-- UFO is a flat green disc
drawUfo :: Ufo -> Picture
drawUfo u = translate x y (color green (scale 1 0.5 (circleSolid (collisionsize b))))
    where
        b      = ubody u
        (x, y) = location b