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

spawnBodyside :: IO Body
spawnBodyside = do
    t <- randomRIO (1, 4) :: IO Int

    x <- randomRIO (-960, 960) :: IO Float
    y <- randomRIO (-540, 540) :: IO Float

    vx <- randomRIO (-30.0, 30.0) :: IO Float
    vy <- randomRIO (-30.0, 30.0) :: IO Float

    let pos
          | t == 1 = (x, 540)
          | t == 2 = (960, y)
          | t == 3 = (x, -540)
          | otherwise = (-960, y)

    return (Body pos (vx, vy) 0 0)

bodycollisioncheck :: Body -> Body -> Bool
bodycollisioncheck b1 b2 = (collisionsize b1 + collisionsize b2) > d
    where
        d = distancepoints (location b1) (location b2)
