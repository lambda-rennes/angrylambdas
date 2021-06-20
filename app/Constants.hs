
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

gravity :: Vect
gravity = Vect 0 (-500)

leftBankAX, leftBankAY, leftBankBX, leftBankBY :: Floating a => a
leftBankAX = -960
leftBankAY = -220
leftBankBX = -310
leftBankBY = -175

rightBankAX, rightBankAY, rightBankBX, rightBankBY :: Floating a => a
rightBankAX = 225
rightBankAY = -150
rightBankBX = 960
rightBankBY = -100

leftGroundA :: Vect
leftGroundB :: Vect
(leftGroundA, leftGroundB) =
  ( Vect leftBankAX leftBankAY,
    Vect leftBankBX leftBankBY
  )

rightGroundA :: Vect
rightGroundB :: Vect
(rightGroundA, rightGroundB) =
  ( Vect rightBankAX rightBankAY,
    Vect rightBankBX rightBankBY
  )