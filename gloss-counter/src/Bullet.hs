module Bullet where
import Body
data Bullet = Bullet
    {
        bbody :: Body
       ,lifetime :: Int
    }

bulletbody :: Body
bulletbody = Body (0,0) (0,0) 0 3

bulletstep :: Bullet -> Bullet
bulletstep b = b {bbody = moveBody (bbody b), lifetime = (lifetime b)- 1}

