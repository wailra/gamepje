module Model where
 
-- | This module contains the data types
--   which represent the state of the game 
import Ship
import Asteroid


data GameState = GameState {
                 elapsedTime :: Float
                 ,ship :: Ship
                 ,asteroids :: [Asteroid]
                 ,spawnTimer :: Float
                 ,lives :: Int
                 }



initialState :: GameState
initialState = GameState 0 (spawnShip) [] 0 3

