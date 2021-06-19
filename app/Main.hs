{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Chiphunk.Low
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad (foldM)
import Data.Function ((&))
import Data.StateVar (StateVar, mapStateVar)
import Foreign.Ptr (nullPtr)
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Rendering
import World
import Utils
import Physics

import Assets (Assets(..))
import qualified Assets
import Constants


main :: IO ()
main = do
  -- Space
  let gravity = Vect 0 (-500)
  space <- createSpace gravity
  collisionQueue <- STM.atomically TQueue.newTQueue
  assets <- Assets.load

  world <- createWorld assets space 
  createCallBacks space collisionQueue


  playIO window black 60 world (render assets) handleEvent (advanceSim space collisionQueue advanceWorld)




window :: Display
window = FullScreen 
-- window = InWindow "Abstract them all" (2000, 1000) (500, 500)


ballInitPos :: Pos
ballInitPos = initSlingshotPos

collisionCallback :: CollisionCallback Bool
collisionCallback arbiter space _ = do
  putStrLn "COLLISION !"
  pure True

enemyCollisionCallback :: TQueue EnemyCollision -> CollisionCallback ()
enemyCollisionCallback collisionQueue arbiter space _ = do
  (enemyCollisionBody, _) <- get $ arbiterBodies arbiter
  enemyCollisionTotalImpulse <- get $ arbiterTotalImpulse arbiter
  STM.atomically $ TQueue.writeTQueue collisionQueue $ EnemyCollision {..}

data BlockDescription = BlockDescription
  { bdescPosition :: Vect,
    bdescDimensions :: Vect,
    bdescAngle :: Double
  }







initBall :: Slingshot
initBall = Slingshot ballRadius initSlingshotPos Free

-- data Object a =
--   Object
--     { data :: a
--     ,  :: Body
--     }

createBlock :: Space -> BlockDescription -> IO Block
createBlock space bdesc = do
  blockBody <- bodyNew blockMass blockMoment
  spaceAddBody space blockBody
  let Vect width height = bdescDimensions bdesc
  blockShape <- boxShapeNew blockBody width height 0
  spaceAddShape space blockShape

  shapeFriction blockShape $= 0.5
  shapeElasticity blockShape $= 0.8

  bodyPosition blockBody $= bdescPosition bdesc
  bodyAngle blockBody $= bdescAngle bdesc
  pure $
    Block
      { blockBody = blockBody,
        blockDimensions = bdescDimensions bdesc
      }
  where
    blockMass = 0.5
    blockMoment =
      momentForBox
        blockMass
        (bdesc & bdescDimensions & vX)
        (bdesc & bdescDimensions & vY)
    blockRadius = 2





-- polygon $
-- rectanglePath (double2Float width) (double2Float height)





handleEvent :: Event -> World -> IO World
handleEvent (EventMotion mousePos@(mX, mY)) world@World {slingshot = (ball@Slingshot {slingshotGrabbed = Grabbed})} =
  do
    let -- Final ball distance from its initial position after grab
        ballDist = min distFromInitPos maxGrabDist
        -- Distance between mouse cursor and initial position
        distFromInitPos = distance ballInitPos mousePos
        -- Unit vector between ball
        v@(vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
        -- New Position
        newPos = (iX + ballDist * vX, iY + ballDist * vY)
        (iX, iY) = ballInitPos

    pure world {slingshot = ball {slingshotPosition = newPos}}
handleEvent (EventKey (Char 'q') Down _ _) _ = exitSuccess
handleEvent
  (EventKey (MouseButton LeftButton) Up _ _)
  world@World {space, slingshot = (ball@Slingshot {slingshotPosition = sPos@(sX, sY), slingshotGrabbed = Grabbed}), thrownBalls} = do
    let d = distance ballInitPos sPos
        initVelocityNorm = maxInitVelocity * d / maxGrabDist
        (bX, bY) = ballInitPos
        v =
          Vect
            (float2Double $ initVelocityNorm * (bX - sX) / d)
            (float2Double $ initVelocityNorm * (bY - sY) / d)

    newBall <- createBall space ballRadius (Vect (float2Double sX) (float2Double sY)) v
    pure $
      world
        { slingshot = initBall,
          thrownBalls = newBall : thrownBalls
        }
handleEvent
  (EventKey (MouseButton LeftButton) Down _ mousePos)
  world@World
    { slingshot =
        ( ball@Slingshot
            { slingshotRadius,
              slingshotPosition,
              slingshotGrabbed = Free
            }
          )
    }
    | grabCircle slingshotRadius slingshotPosition mousePos == True = do
      let world' = world {slingshot = ball {slingshotGrabbed = Grabbed}}
      -- print $ show $ world'
      pure world'
handleEvent _ world = pure world



grabCircle :: Radius -> Pos -> Pos -> Bool
grabCircle radius circlePos clickPos = radius >= distance circlePos clickPos

distance :: Pos -> Pos -> Float
distance (x1, y1) (x2, y2) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

advanceWorld :: Float -> World -> World
advanceWorld _ world = world

advanceSim ::
  Space ->
  TQueue EnemyCollision ->
  (Float -> World -> World) ->
  Float ->
  World ->
  IO World
advanceSim space collisionQueue advance tic world = do
  collisions <- STM.atomically $ TQueue.flushTQueue collisionQueue
  let handleCollision world EnemyCollision {enemyCollisionBody, enemyCollisionTotalImpulse} = do
        let impulse = (vX enemyCollisionTotalImpulse) ** 2 + (vY enemyCollisionTotalImpulse) ** 2
        case impulse > 100000 of
          True -> do
            let iterFunc body shape _ =
                  spaceRemoveShape space shape
            bodyEachShape enemyCollisionBody iterFunc nullPtr
            spaceRemoveBody space enemyCollisionBody
            pure $ world {enemies = filter (/= enemyCollisionBody) (enemies world)}
          False ->
            pure world

  world' <- foldM handleCollision world collisions
  spaceStep space (1 / 60)
  pure $ advance tic world'

-- renderBody :: Picture -> Body -> IO Picture




(leftGroundA, leftGroundB) =
  ( Vect leftBankAX leftBankAY,
    Vect leftBankBX leftBankBY
  )



(rightGroundA, rightGroundB) =
  ( Vect rightBankAX rightBankAY,
    Vect rightBankBX rightBankBY
  )

logX, logY :: Floating a => a
logX = 0
logY = -145



createLog :: Space -> Picture -> BoxInfo Float -> Pos -> IO Log
createLog space pic boxInfo pos = do
  logBody <- createBox space boxInfo pos
  pure $ Log' pic logBody


createWorld :: Assets -> Space -> IO World
createWorld assets@Assets{woodenLog} space = do
  -- Ground
  _ <- createGround space

  -- Log

  let logInfo = BoxInfo 0.5 (700, 200)
  let logPos = 
  logObj <- createLog space woodenLog  (0, -145)

  -- Blocks
  blocks <- traverse (createBlock space) blockDescriptions

  -- Ball
  -- ballBody <- createBall space ballRadius (Vect (-100) 300) (Vect 200 0)
  enemy <- createEnemy space ballRadius (Vect 300 400) (Vect 0 0)

  

  -- logImg <- lambda <- loadBMP "imgs/lambda.bmp"

  -- World
  pure $ World space blocks initBall logObj [] [enemy]


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

blockDescriptions :: [BlockDescription]
blockDescriptions =
  [ BlockDescription
      { bdescPosition = Vect 250 (-25),
        bdescDimensions = Vect 25 250,
        bdescAngle = 0
      },
    BlockDescription
      { bdescPosition = Vect 350 (-25),
        bdescDimensions = Vect 25 250,
        bdescAngle = 0
      },
    BlockDescription
      { bdescPosition = Vect 300 (70),
        bdescDimensions = Vect 25 250,
        bdescAngle = 3.1415 / 2
      }
  ]





createBall :: Space -> Double -> Vect -> Vect -> IO Ball
createBall space radius initPos initVelocity = do
  let moment = momentForCircle ballMass 0 radius (Vect 0 0)

  ballBody <- bodyNew ballMass moment
  spaceAddBody space ballBody
  bodyPosition ballBody $= initPos
  bodyVelocity ballBody $= initVelocity

  ballShape <- circleShapeNew ballBody radius (Vect 0 0)

  shapeFriction ballShape $= ballFriction
  shapeElasticity ballShape $= 0.9

  shapeCollisionType' ballShape $= BallCT

  spaceAddShape space ballShape

  pure ballBody

createEnemy :: Space -> Double -> Vect -> Vect -> IO Ball
createEnemy space radius initPos initVelocity = do
  let moment = momentForCircle ballMass 0 radius (Vect 0 0)

  enemyBody <- bodyNew ballMass moment
  spaceAddBody space enemyBody
  bodyPosition enemyBody $= initPos
  bodyVelocity enemyBody $= initVelocity

  enemyShape <- circleShapeNew enemyBody radius (Vect 0 0)

  shapeFriction enemyShape $= 0.7
  shapeElasticity enemyShape $= 0

  shapeCollisionType' enemyShape $= EnemyCT

  spaceAddShape space enemyShape

  pure enemyBody

createCallBacks :: Space -> TQueue EnemyCollision -> IO ()
createCallBacks space collisionQueue = do
  callback <- mkCallback (enemyCollisionCallback collisionQueue)
  colHandlerPtr <- spaceAddWildcardHandler' space EnemyCT
  modifyCollisionHandler colHandlerPtr $ \colHandler -> pure $ colHandler {chPostSolveFunc = callback}

  pure ()
