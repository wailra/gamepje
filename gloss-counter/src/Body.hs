module Body where
import System.Random
import Math
data Body = Body
    { location :: (Float,Float)
    , velocity :: (Float,Float)
    , direction :: Float
    , collisionsize :: Float
    }

moveBody :: Body -> Body
moveBody b = b { location = ((fst (location b)) + (fst (velocity b)) ,(snd (location b)) + (snd (velocity b)))}

boundingboxBody :: Body -> Body
boundingboxBody b | ((fst (location b)) >= 960) = b {location = ((fst (location b) ) - 1920, snd (location b))}
boundingboxBody b | ((fst (location b)) <= -960) = b  {location = ((fst (location b)) + 1920, snd (location b))}
boundingboxBody b | ((snd (location b)) >= 540) = b {location = ((fst (location b)) , (snd (location b)) - 1080)}
boundingboxBody b | ((snd (location b)) <= -540) = b {location = ((fst (location b)) , (snd (location b))+ 1080)}
                  | otherwise = b

spawnBodyside :: StdGen -> (Body, StdGen)
spawnBodyside gen =
    let
        (t, gen1) = randomR (1, 4) gen :: (Int, StdGen)
        (x, gen2)  = randomR (-960, 960) gen1
        (y, gen3)  = randomR (-540, 540) gen2
        (vx, gen4) = randomR (-30.0, 30.0) gen3
        (vy, gen5) = randomR (-30.0, 30.0) gen4

        pos
          | t == 1    = (x, 540)
          | t == 2    = (960, y)
          | t == 3    = (x, -540)
          | otherwise = (-960, y)

    in
        (Body pos (vx, vy) 0 0, gen5)

bodycollisioncheck :: Body -> Body -> Bool
bodycollisioncheck b1 b2 = (collisionsize b1 + collisionsize b2) > d
    where
        d = distancepoints (location b1) (location b2)

-- True if body b touches one of the bodies in the list
touchesany :: [Body] -> Body -> Bool
touchesany bodies b = any (bodycollisioncheck b) bodies