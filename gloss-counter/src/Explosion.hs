module Explosion where

-- An explosion is a ring that gets big and goes out again where something is destroyed
data Explosion = Explosion
    { epos  :: (Float, Float)   
    , etime :: Float            
    }

-- How long one explosion lasts in sec
explosionlength :: Float
explosionlength = 0.8

-- New explosion on that place starting at time 0
newexplosion :: (Float, Float) -> Explosion
newexplosion p = Explosion p 0

-- One frame later, the explosion gets dt seconds added
explosionstep :: Float -> Explosion -> Explosion
explosionstep dt e = e { etime = etime e + dt }

-- | True so far as the explosion isn't finished
explosionalive :: Explosion -> Bool
explosionalive e = etime e < explosionlength