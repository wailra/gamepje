module Main where

import Controller
import Model
import View
import Ship
import System.IO (readFile')

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    appendFile "highscores.txt" ""            -- if not there make one
    contents <- readFile' "highscores.txt"    -- read all that's saved
    let best = maximum (0 : map read (lines contents))   -- the highest one (0 if there isn't anu)
    playIO (InWindow "Counter" (1920, 1080) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState best) -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function