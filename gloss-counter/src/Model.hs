module Model where
 
-- | This module contains the data types
--   which represent the state of the game 
import Ship
import Asteroid
import Hunter
import Ufo
import Bullet
import Explosion
import System.Random
import Data.Bool (Bool(False))


data GameState = GameState {
                 elapsedTime :: Float
                 ,ship :: Ship
                 ,asteroids :: [Asteroid]
                 ,spawnTimer :: Float
                 ,lives :: Int
                 ,hunters :: [Hunter]
                 ,ufos :: [Ufo]
                 ,ufobullets :: [Bullet]
                 ,score :: Int
                 ,gameover :: Bool
                 ,explosions :: [Explosion]
                 ,generator :: StdGen
                 ,paused :: Bool
                 ,highscore :: Int
                 }



initialState :: Int -> GameState
initialState best = GameState 0 (spawnShip) [] 0 3 [] [] [] 0 False [] (mkStdGen 42) False best

