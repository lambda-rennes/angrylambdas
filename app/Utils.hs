module Utils 
  ( module GHC.Float
  , rad2deg
  , vect2Pos
  , pos2Vect
  ) where

import GHC.Float (double2Float, float2Double)
import Chiphunk.Low

rad2deg :: Fractional a => a -> a
rad2deg x = x * 180 / 3.1415

vect2Pos :: Vect -> (Float, Float)
vect2Pos (Vect x y) = (double2Float x, double2Float y)

pos2Vect :: (Float, Float) -> Vect
pos2Vect (x, y) = Vect (float2Double x) (float2Double y)
