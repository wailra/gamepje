module Model where
 
-- | This module contains the data types
--   which represent the state of the game 
import Ship
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                 elapsedTime :: Float
                 , ship :: Ship
                 }



initialState :: GameState
initialState = GameState 0 spawnship