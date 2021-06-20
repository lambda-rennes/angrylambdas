{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module World where

import Chiphunk.Low
import Graphics.Gloss (Picture)

import Assets
import Physics
import Collisions
import Constants

-- World record
data World = World
  { space :: Space,
    slingshot :: Slingshot,
    log' :: Log,
    thrownBalls :: [Ball],
    enemies :: [Enemy]
  }

-- type aliases for Objects
newtype Log = Log GameObject

newtype Ball = Ball GameObject

newtype Enemy = Enemy GameObject

newtype Block = Block GameObject

data GameObject = GameObject
  { objPicture :: Picture,
    objBody :: Body
  }

-- Slingshot
data Slingshot = Slingshot
  { slingshotRadius :: Float,
    slingshotBallRadius :: Float,
    slingshotCenter :: Pos,
    slingshotState :: Grabbed Pos
  }
  deriving (Show)

-- Utils types
type Pos = (Float, Float)

data Grabbed a = Grabbed a | Free deriving (Show)

instance Functor Grabbed where
  fmap f Free = Free
  fmap f (Grabbed a) = Grabbed $ f a

instance Foldable Grabbed where
  foldMap f Free = mempty
  foldMap f (Grabbed a) = f a

instance Traversable Grabbed where
  traverse f Free = pure Free
  traverse f (Grabbed a) = Grabbed <$> f a

type Radius = Float

type Gravity = Vect

-- **** Objects creation *****

createWorld :: Assets -> Space -> IO World
createWorld Assets {woodenLog, monsterBind} space = do
  
  -- Ground
  _ <- createGround space

  -- Log
  logObj <- createLog space woodenLog logInfo logPos

  -- Enemy
  enemy <- createEnemy space monsterBind enemyInfo enemyPos (50, 0)

  -- World
  pure $ World space initSlingshot logObj [] [enemy]

  where 
    logInfo :: BoxInfo Float
    logInfo =  BoxInfo
               { boxMass = 2,
                 boxSize = (700, 90),
                 boxFriction = 0.5,
                 boxElasticity = 0.8
               } 
    logPos :: Pos
    logPos = (0, -80)
    enemyInfo :: DiscInfo Float
    enemyInfo = DiscInfo
                { discRadius = ballRadius,
                  discElasticity = 0.9,
                  discFriction = 5,
                  discMass = 5
                }
    enemyPos :: Pos
    enemyPos = (350, 60)
    initSlingshot :: Slingshot
    initSlingshot = Slingshot
                    { slingshotBallRadius = ballRadius
                    , slingshotRadius = 300
                    , slingshotCenter = (-700, 30)
                    , slingshotState = Free
                    }


createGround :: Space -> IO ()
createGround space = do
  -- Create Space body
  spaceBody <- get $ spaceStaticBody space

  -- LeftBank
  leftGround <- segmentShapeNew spaceBody leftGroundA leftGroundB 0
  shapeFriction leftGround $= groundFriction
  shapeElasticity leftGround $= 0.9

  spaceAddShape space leftGround
  shapeCollisionType' leftGround $= GroundCT

  --Right Bank
  rightGround <- segmentShapeNew spaceBody rightGroundA rightGroundB 0
  shapeFriction rightGround $= groundFriction
  shapeElasticity rightGround $= 0.9

  spaceAddShape space rightGround
  shapeCollisionType' rightGround $= GroundCT

  pure ()

createLog :: Space -> Picture -> BoxInfo Float -> Pos -> IO Log
createLog space logPicture boxInfo pos = do
  objBody <- createBox space boxInfo pos
  pure $
    Log $
      GameObject
        { objPicture = logPicture,
          objBody
        }

createSpace :: Gravity -> IO Space
createSpace g = do
  space <- spaceNew
  spaceGravity space $= g
  pure space

createBlock :: Space -> Picture -> BoxInfo Float -> Pos -> IO Block
createBlock space objPicture boxInfo pos = do
  objBody <- createBox space boxInfo pos
  pure $
    Block $ GameObject {..}

createBall :: Space -> Picture -> DiscInfo Float -> Pos -> (Float, Float) -> IO Ball
createBall space objPicture discInfo pos velocity = do
  objBody <- createDisc space discInfo pos velocity
  pure $ Ball $ GameObject {..}

createEnemy :: Space -> Picture -> DiscInfo Float -> Pos -> (Float, Float) -> IO Enemy
createEnemy space objPicture discInfo pos velocity = do
  objBody <- createDisc space discInfo pos velocity
  pure $ Enemy $ GameObject {..}


distance :: Pos -> Pos -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

advanceWorld :: Float -> World -> World
advanceWorld _ world = world

instance Show Body where
  show _ = "Body..."

instance Show Space where
  show _ = "Space.."
