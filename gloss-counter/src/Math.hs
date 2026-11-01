module Math where

translation :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
translation (xo, yo) rotation (x, y) = (xn + xo, yn + yo)
  where
    xn = x * cos rotation + y * sin rotation
    yn = -x * sin rotation + y * cos rotation

degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * pi / 180