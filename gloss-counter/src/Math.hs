

translation :: (Float,Float) -> (Float,Float) -> Float -> (Float, Float)
translation (x,y) (xo,yo) rotation = (xn + ox, yn + yo)
where 
    xn = x' * cos(rotation) - y'sin(rotation)
    yn = x' * sin(rotation) + y'cos(rotation)
    x' = x - xo
    y' = y - yo
