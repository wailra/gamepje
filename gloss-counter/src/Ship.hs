{-# language NamedFieldPuns #-}
module Ship where

import Math

data Ship = Ship
  { location :: (Float,Float)
  , direction  :: Float
  , velocity :: (Float, Float)
  , upkey :: Bool
  , leftkey :: Bool
  , rightkey :: Bool
  }

moveship :: Ship -> Ship
moveship s = s { location = ((fst (location s)) + (fst (velocity s)) ,(snd (location s)) + (snd (velocity s)))}

shipwidth :: Float
shipwidth = 50.0

shiplength :: Float
shiplength = 100.0

dragship :: Ship -> Ship
dragship s = s {velocity = ((fst (velocity s)) * drag, (snd (velocity s)) * drag)}
 where
  drag = 0.99

shiptopoints :: Ship -> [(Float,Float)]
shiptopoints s = 
  map (translation (location s) (degreesToRadians (direction s))) basicpoints
  where
   basicpoints =
    [ (shipwidth / 2,  shiplength / 2)
      , ( shipwidth / 2,  shiplength / 2)
      , ( 0,             -shiplength / 2)
    ]

spawnship :: Ship
spawnship = Ship (50.0 , 50.0) 0 (0,0) False False False-- degrees, 0 = pointing upwards 0 = velocity


