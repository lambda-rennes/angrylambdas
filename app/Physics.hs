{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}


module Physics where

import Chiphunk.Low
import Data.StateVar
import Utils

import Collisions

data BoxInfo a = 
    BoxInfo { boxMass :: a
            , boxSize :: (a, a)
            , boxFriction :: a
            , boxElasticity :: a
            } deriving (Show, Functor)

data DiscInfo a =
    DiscInfo
      { discMass :: a
      , discRadius :: a
      , discFriction :: a
      , discElasticity :: a
      } deriving (Show, Functor)

createBox :: Space -> BoxInfo Float -> (Float, Float) -> IO Body
createBox space boxInfo pos@(x, y) = do
  let BoxInfo{boxMass, boxSize = (w, h), boxFriction, boxElasticity} = float2Double <$> boxInfo
      boxMoment = momentForBox boxMass (float2Double x) (float2Double y)

  -- New Body
  body <- bodyNew boxMass boxMoment
  bodyPosition' body $= pos
  bodyAngle body $= 0

  -- Add body to space
  spaceAddBody space body

  -- New Shape
  shape <- boxShapeNew body w h 0
  shapeFriction shape $= boxFriction
  shapeElasticity shape $= boxElasticity

  -- Add shape to body
  spaceAddShape space shape

  pure body


createDisc :: Space -> DiscInfo Float -> (Float, Float) -> (Float, Float) -> IO Body
createDisc space discInfo pos velocity = do
  let DiscInfo{discMass, discRadius, discFriction, discElasticity} = float2Double <$> discInfo
      ballMoment = momentForCircle discMass 0 discRadius (Vect 0 0)

  -- New Body
  body <- bodyNew discMass ballMoment
  bodyPosition' body $= pos
  bodyVelocity' body $= velocity
  bodyAngle body $= 0

  -- New shape
  ballShape <- circleShapeNew body discRadius (Vect 0 0)

  shapeFriction ballShape $= discFriction
  shapeElasticity ballShape $= discElasticity

  shapeCollisionType' ballShape $= BallCT

  spaceAddShape space ballShape
  spaceAddBody space body

  pure body

bodyPosition' :: Body -> StateVar (Float, Float)
bodyPosition' body = mapStateVar to from (bodyPosition body)
  where from (Vect x y) = (double2Float x, double2Float y)
        to (x, y) = Vect (float2Double x) (float2Double y)


bodyVelocity' :: Body -> StateVar (Float, Float)
bodyVelocity' body = mapStateVar to from (bodyVelocity body)
  where from (Vect x y) = (double2Float x, double2Float y)
        to (x, y) = Vect (float2Double x) (float2Double y)