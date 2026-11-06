module Model where
 
-- | This module contains the data types
--   which represent the state of the game 
import Ship
import Asteroid
import Hunter
import Ufo
import Bullet


data GameState = GameState {
                 elapsedTime :: Float
                 ,ship :: Ship
                 ,asteroids :: [Asteroid]
                 ,spawnTimer :: Float
                 ,lives :: Int
                 ,hunters :: [Hunter]
                 ,ufos :: [Ufo]
                 ,ufobullets :: [Bullet]
                 }



initialState :: GameState
initialState = GameState 0 (spawnShip) [] 0 3 [] [] []

