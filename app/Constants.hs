
module Constants where

import Chiphunk.Low ( Vect(Vect) )

groundFriction :: Floating a => a
groundFriction = 1

ballRadius :: Floating a => a
ballRadius = 50

ballMass :: Floating a => a
ballMass = 1

ballFriction :: Floating a => a
ballFriction = 0.7

maxInitVelocity :: Floating a => a
maxInitVelocity = 1000