{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module World where

import Control.Monad (forM)

import Chiphunk.Low
import Graphics.Gloss (Picture)

import Assets
import Physics
import Collisions
import Constants

data World = World
  { space :: Space,
    blocks :: [Block],
    slingshot :: Slingshot,
    log' :: Log,
    thrownBalls :: [Ball],
    enemies :: [Enemy]
  }

data Slingshot = Slingshot
  { slingshotRadius :: Float,
    slingshotBallRadius :: Float,
    slingshotCenter :: Pos,
    slingshotState :: Grabbed
  }
  deriving (Show)

newtype Log = Log GameObject

newtype Ball = Ball GameObject

newtype Enemy = Enemy GameObject

newtype Block = Block GameObject

data GameObject = GameObject
  { objPicture :: Picture,
    objBody :: Body
  }

type Pos = (Float, Float)

data Grabbed = Grabbed Pos | Free deriving (Show)

type Radius = Float

instance Show Body where
  show _ = "Body..."

instance Show Space where
  show _ = "Space.."

initSlingshotPos :: Num a => (a, a)
initSlingshotPos = (-350, 200)

leftBankAX, leftBankAY, leftBankBX, leftBankBY :: Floating a => a
leftBankAX = -1000
leftBankAY = -220
leftBankBX = -310
leftBankBY = -175

rightBankAX, rightBankAY, rightBankBX, rightBankBY :: Floating a => a
rightBankAX = 225
rightBankAY = -150
rightBankBX = 1000
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

logX, logY :: Floating a => a
logX = 0
logY = -145

createLog :: Space -> Picture -> BoxInfo Float -> Pos -> IO Log
createLog space logPicture boxInfo pos = do
  objBody <- createBox space boxInfo pos
  pure $
    Log $
      GameObject
        { objPicture = logPicture,
          objBody
        }

createWorld :: Assets -> Space -> IO World
createWorld assets@Assets {woodenLog, wood, monsterBind} space = do
  -- Ground
  _ <- createGround space

  -- Log
  let logInfo =
        BoxInfo
          { boxMass = 0.5,
            boxSize = (700, 200),
            boxFriction = 0.5,
            boxElasticity = 0.8
          }

  let logPos = (0, -145)

  logObj <- createLog space woodenLog logInfo logPos

  -- Blocks
  let boxInfo =
        BoxInfo
          { boxMass = 0.5,
            boxFriction = 0.5,
            boxElasticity = 0.8,
            boxSize = (25, 250)
          }
      blocks =
        [ (boxInfo, (250, -25), 0),
          (boxInfo, (350, -25), 0),
          (boxInfo, (25, 250), 3.1415 / 2)
        ]

  blocks <- forM blocks $ \(box, pos, _) ->
    createBlock space wood box pos

  -- Ball
  -- ballBody <- createBall space ballRadius (Vect (-100) 300) (Vect 200 0)
  let discInfo =
        DiscInfo
          { discRadius = ballRadius,
            discElasticity = 0.9,
            discFriction = 5,
            discMass = 5
          }
  enemy <- createEnemy space monsterBind discInfo (300, 400) (50, 0)

  -- logImg <- lambda <- loadBMP "imgs/lambda.bmp"

  -- World
  pure $ World space blocks initSlingshot logObj [] [enemy]

createGround :: Space -> IO ()
createGround space = do
  -- Create Space body
  spaceBody <- get $ spaceStaticBody space

  -- LeftBank
  leftGround <- segmentShapeNew spaceBody leftGroundA leftGroundB 0
  shapeFriction leftGround $= groundFriction
  shapeElasticity leftGround $= 0.9
  groundCollisionType <- get $ shapeCollisionType leftGround

  spaceAddShape space leftGround
  shapeCollisionType' leftGround $= GroundCT

  --Right Bank
  rightGround <- segmentShapeNew spaceBody rightGroundA rightGroundB 0
  shapeFriction rightGround $= groundFriction
  shapeElasticity rightGround $= 0.9
  groundCollisionType <- get $ shapeCollisionType rightGround

  spaceAddShape space rightGround
  shapeCollisionType' rightGround $= GroundCT

  pure ()

type Gravity = Vect

createSpace :: Gravity -> IO Space
createSpace gravity = do
  space <- spaceNew
  spaceGravity space $= gravity
  pure space


ballInitPos :: Pos
ballInitPos = initSlingshotPos

initSlingshot :: Slingshot
initSlingshot = Slingshot
  { slingshotBallRadius = ballRadius
  , slingshotRadius = 300
  , slingshotCenter = (-1000, 200)
  , slingshotState = Free
  }

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