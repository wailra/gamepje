module Math where

translation :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
translation (xo, yo) rotation (x, y) = (xn + xo, yn + yo)
  where
    xn = x * cos rotation + y * sin rotation
    yn = -x * sin rotation + y * cos rotation

degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * pi / 180

distancepoints :: (Float, Float) -> (Float, Float) -> Float
distancepoints (x1, y1) (x2, y2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2)

collisioncheck :: Float -> Float -> Float -> Bool
collisioncheck s1 s2 d = (s1 + s2) < d

tupleMutiply :: (Float, Float) -> Float -> (Float,Float)
tupleMutiply (a,b) c = (a*c,a*b)