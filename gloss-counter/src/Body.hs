module Body where

import Controller
import Model
import View

data Body = Body
  { location : (Float,Float)
    size : Float
  }

shipbody :: Body
shipbody = Body (50.0,50,0) 50


