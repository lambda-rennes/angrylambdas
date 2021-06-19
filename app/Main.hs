{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Chiphunk.Low
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad (foldM, forM)
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


  playIO window black 60 world (render assets) (handleEvent assets) (advanceSim space collisionQueue advanceWorld)




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

createBlock' :: Space -> Picture -> BoxInfo Float -> Pos -> IO Block' 
createBlock' space blockImg boxInfo pos = do
  blockBody <- createBox space boxInfo pos
  pure $ Block' { blockPicture = blockImg
               , blockBody = blockBody 
               }

createBall :: Space -> Picture -> DiscInfo Float -> Pos -> (Float, Float) -> IO Ball'
createBall space ballPicture discInfo pos velocity = do
  ballBody <- createDisc space discInfo pos velocity
  pure $ Ball' { .. }






handleEvent :: Assets -> Event -> World -> IO World
handleEvent _ (EventMotion mousePos@(mX, mY)) world@World {slingshot = (ball@Slingshot {slingshotGrabbed = Grabbed})} =
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
handleEvent _ (EventKey (Char 'q') Down _ _) _ = exitSuccess
handleEvent
  Assets{..}
  (EventKey (MouseButton LeftButton) Up _ _)
  world@World {space, slingshot = (ball@Slingshot {slingshotPosition = sPos@(sX, sY), slingshotGrabbed = Grabbed}), thrownBalls} = do
    let d = distance ballInitPos sPos
        initVelocityNorm = maxInitVelocity * d / maxGrabDist
        (bX, bY) = ballInitPos
        v =
            ( initVelocityNorm * (bX - sX) / d
            , initVelocityNorm * (bY - sY) / d
            )

    let discInfo = DiscInfo
             { discRadius = ballRadius
             , discElasticity = 0.9
             , discFriction = 0
             , discMass = 5
             }
    newBall <- createBall space lambdaBall discInfo (sX, sY) v
    pure $
      world
        { slingshot = initBall,
          thrownBalls = newBall : thrownBalls
        }
handleEvent
  _
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
handleEvent _ _ world = pure world



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
  pure $ Log { logPicture = pic 
             , logBody = logBody
             }


createWorld :: Assets -> Space -> IO World
createWorld assets@Assets{woodenLog, wood} space = do
  -- Ground
  _ <- createGround space

  -- Log
  let logInfo = BoxInfo { boxMass = 0.5 
                        , boxSize = (700, 200)
                        , boxFriction = 0.5 
                        , boxElasticity = 0.8
                        }

  let logPos = (0, -145)

  logObj <- createLog space woodenLog logInfo logPos 

  -- Blocks
  let boxInfo = BoxInfo
        { boxMass = 0.5
        , boxFriction = 0.5
        , boxElasticity = 0.8
        , boxSize = (25,250)
        }
      blocks =
        [ ( boxInfo,  (250, -25), 0 ),
          ( boxInfo, (350, -25), 0 ),
          ( boxInfo, (25, 250), 3.1415 / 2 )
        ]

  blocks <- forM blocks $ \(box, pos, _) -> do
    createBlock' space wood box pos

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
