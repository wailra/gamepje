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
import Explosion

view :: GameState -> IO Picture
view gstate
    | gameover gstate = return (drawGameOver (score gstate))   -- game over screen
    | otherwise       = return (viewPure gstate)               -- normal game

viewPure :: GameState -> Picture
viewPure gstate = pictures
    [ color white (polygon (shiptopoints s))
    , pictures (map drawAsteroid (asteroids gstate))
    , pictures (map drawBullet (bullets (ship gstate)))
    , pictures (map drawHunter (hunters gstate))
    , pictures (map drawUfo (ufos gstate))
    , pictures (map drawBullet (ufobullets gstate))
    , pictures (map drawExplosion (explosions gstate))
    , drawScore (score gstate)
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

-- The score shown in white on top left corner
drawScore :: Int -> Picture
drawScore sc = translate (-940) 480 (scale 0.3 0.3 (color white (text ("Score: " ++ show sc))))

-- The game over screen is a big red text with your score and displaying how to play again
drawGameOver :: Int -> Picture
drawGameOver sc = pictures
    [ translate (-380) 50 (color red (text "GAME OVER"))
    , translate (-160) (-80) (scale 0.4 0.4 (color white (text ("Score: " ++ show sc))))
    , translate (-250) (-180) (scale 0.3 0.3 (color white (text "Press Enter to play again")))
    ]

-- An explosion is a yellow ring that gets big and then fades out
drawExplosion :: Explosion -> Picture
drawExplosion e = translate x y (color c (thickCircle radius 6))
    where
        (x, y)   = epos e
        progress = etime e / explosionlength      -- 0 at the start, 1 at the end
        radius   = 10 + 60 * progress             
        c        = withAlpha (1 - progress) yellow -- from solid to gone