{-# language NamedFieldPuns #-}
module Ship where

import Math
import Body
import Bullet

data Ship = Ship
  { sbody :: Body
  , upkey :: Bool
  , leftkey :: Bool
  , rightkey :: Bool
  , spacebar :: Bool
  , bullets :: [Bullet]
  }

shipBody :: Body
shipBody = (Body (0,0) (0,0) 0 20)

spawnShip :: Ship
spawnShip = Ship shipBody False False False False []

shipwidth :: Float
shipwidth = 50.0

shiplength :: Float
shiplength = 100.0

dragship :: Ship -> Ship
dragship s = s {sbody = (sbody s) {velocity = ((fst (velocity (sbody s))) * drag, (snd (velocity (sbody s))) * drag)}}
 where
  drag = 0.9

boostship :: Ship -> Ship
boostship s | up = s { sbody =  b{velocity = ((-sin dir) * 5 + (fst vel) , (-cos dir) * 5 + (snd vel) )}}
            | otherwise = s
            where
  up = upkey s
  dir = degreesToRadians (direction b)
  vel = velocity b
  b = sbody s

turnship :: Ship -> Ship 
turnship s | leftkey s = s {sbody = (sbody s){direction = (direction (sbody s)) - 10}}
           | rightkey s = s {sbody = (sbody s){direction = (direction (sbody s)) + 10}}
           | otherwise = s 

shipstep :: Ship -> Ship
shipstep s =
  let s' = shipshoot (dragship (boostship (turnship s)))
  in s' {sbody = boundingboxBody (moveBody (sbody s')),bullets = filter lifetimecheck (map bulletstep (bullets s'))}

lifetimecheck :: Bullet -> Bool
lifetimecheck b = ((lifetime b) > 0)

shiptopoints :: Ship -> [(Float,Float)]
shiptopoints s =
  map (translation (location b) (degreesToRadians (direction b))) basicpoints
  where
    basicpoints =
      [ (-shipwidth / 2, shiplength / 2)
      , ( shipwidth / 2, shiplength / 2)
      , ( 0, -shiplength / 2)
      ]
    b = sbody s


shipshoot :: Ship -> Ship
shipshoot s= if (spacebar s)
  then s{bullets = (Bullet ( bulletbody {velocity = (velx + bulx,vely + buly),location = tipofship}) timealive ) : (bullets s)}
    else s
    where
      velx = fst (velocity (sbody s))
      vely = snd (velocity (sbody s))
      bulx = spd * (-sin(dir))
      buly = spd * (-cos(dir))
      dir = degreesToRadians (direction (sbody s))
      locx = fst (location (sbody s))
      locy = snd (location (sbody s))
      tipofship = (shiptopoints s) !! 2 
      timealive = 300
      spd = 20
