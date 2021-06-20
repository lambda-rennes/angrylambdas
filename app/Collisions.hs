{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Collisions where

import Chiphunk.Low
import Data.StateVar

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import GHC.Float

data CollisionType'
  = DefaultCT
  | GroundCT
  | BlockCT
  | BallCT
  | EnemyCT
  deriving (Eq, Enum, Show)

data CollisionObject = CollisionObject
  { objectBody :: Body
  , objectCollisionType :: CollisionType'
  }
  deriving (Eq)

data Collision = Collision
  { collisionObjA :: CollisionObject
  , collisionObjB :: CollisionObject
  , collisionTotalImpulse :: (Float, Float)
  , collisionTotalKineticEnergy :: Float
  }


shapeCollisionType' :: Shape -> StateVar CollisionType'
shapeCollisionType' =
  mapStateVar
    (toEnum . fromEnum)
    (toEnum . fromEnum)
    . shapeCollisionType

createCollisionQueue :: Space ->  IO (TQueue Collision)
createCollisionQueue space = do
    queue <- STM.atomically TQueue.newTQueue
    createCollisionCallback space queue
    pure queue

createCollisionCallback :: Space -> TQueue Collision -> IO ()
createCollisionCallback space collisionQueue = do
  callback <- mkCallback (catchallCallback collisionQueue)
  colHandlerPtr <- spaceAddDefaultCollisionHandler space 
  modifyCollisionHandler colHandlerPtr $ \colHandler -> pure $ colHandler {chPostSolveFunc = callback}

  pure ()

catchallCallback :: TQueue Collision -> CollisionCallback ()
catchallCallback collisionQueue arbiter space _ = do
  (body1, body2) <- get $ arbiterBodies arbiter
  (shape1, shape2) <- get $ arbiterShapes arbiter
  collType1 <- get $ shapeCollisionType' shape1
  collType2 <- get $ shapeCollisionType' shape2

  Vect fx fy <- get $ arbiterTotalImpulse arbiter
  kineticEnergyDouble <- get $ arbiterTotalKE arbiter 

  let collisionTotalImpulse = (double2Float  fx, double2Float fy)
      collisionTotalKineticEnergy = double2Float kineticEnergyDouble

  enemyCollisionTotalImpulse <- get $ arbiterTotalImpulse arbiter
  STM.atomically $ TQueue.writeTQueue collisionQueue $ Collision
    { collisionObjA = CollisionObject
        { objectBody = body1
        , objectCollisionType = collType1
        }
    , collisionObjB = CollisionObject
        { objectBody = body2
        , objectCollisionType = collType2
        }
    , collisionTotalImpulse
    , collisionTotalKineticEnergy
    }