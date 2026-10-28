module Ship where

import Body

{-# language NamedFieldPuns #-}

data Ship = Ship
  { location :: (Float)
  , direction      :: Float
  , velocity :: (Float, Float)
  }
  moveship :: Ship -> Ship
  moveship s = s {location = ((fst location ship) + (fst velocity ship) ,(snd location ship) + (snd velocity ship))}

  

  dragship :: Ship -> Ship
  dragship s = s {velocity = (fst velocity * drag, (snd velocity) * drag)}
    where
        drag = 0.99


 

  spawnship :: Ship
  spawnship = Ship shipbody 0 -- degrees, 0 = pointing upwards


