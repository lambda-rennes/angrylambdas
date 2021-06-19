{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Assets (Assets (..))
import qualified Assets
import Chiphunk.Low
import Collisions
import Constants
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
import Physics
import Rendering
import System.Exit
import Utils
import World

gameLoop ::
  Space ->
  World ->
  TQueue Collision ->
  (World -> IO Picture) ->
  (Event -> World -> IO World) ->
  (Collision -> World -> IO World) ->
  (Float -> World -> IO World) ->
  IO ()
gameLoop space initialWorld collisionQueue render processEvent processCollision advance =
  playIO window black 60 initialWorld render processEvent advance'
  where
    advance' _ world = do
      spaceStep space (1 / 60)
      collisions <- STM.atomically $ TQueue.flushTQueue collisionQueue
      foldM (flip processCollision) world collisions

main :: IO ()
main = do
  -- Space
  let gravity = Vect 0 (-500)
  space <- createSpace gravity
  assets <- Assets.load
  world <- createWorld assets space
  collisionQueue <- createCollisionQueue space

  gameLoop space world collisionQueue (render assets) (handleEvent assets) (flip (handleCollision space)) (\_ world -> pure world) -- (advanceSim space collisionQueue advanceWorld)

window :: Display
window = FullScreen

-- window = InWindow "Abstract them all" (2000, 1000) (500, 500)

clipSlingshotPosition :: Slingshot -> Pos -> Pos
clipSlingshotPosition Slingshot {slingshotCenter, slingshotRadius} mousePos@(mX, mY) =
  let -- Final ball distance from its initial position after grab
      ballDist = min distFromInitPos slingshotRadius
      -- Distance between mouse cursor and initial position
      distFromInitPos = distance slingshotCenter mousePos
      -- Unit vector between ball
      v@(vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
      (iX, iY) = slingshotCenter
   in (iX + ballDist * vX, iY + ballDist * vY)

handleEvent :: Assets -> Event -> World -> IO World
handleEvent _ (EventMotion mousePos@(mX, mY)) world@World {slingshot = slingshot@Slingshot {slingshotRadius, slingshotCenter, slingshotState = Grabbed _}} =
  do
    let -- Final ball distance from its initial position after grab
        ballDist = min distFromInitPos slingshotRadius
        -- Distance between mouse cursor and initial position
        distFromInitPos = distance slingshotCenter mousePos
        -- Unit vector between ball
        v@(vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
        -- New Position
        newPos = (iX + ballDist * vX, iY + ballDist * vY)
        (iX, iY) = slingshotCenter

    pure world {slingshot = slingshot {slingshotState = Grabbed newPos}}
handleEvent _ (EventKey (Char 'q') Down _ _) _ = exitSuccess
handleEvent
  Assets {..}
  (EventKey (MouseButton LeftButton) Up _ _)
  world@World
    { space,
      slingshot = slingshot@Slingshot {slingshotCenter, slingshotRadius, slingshotState = Grabbed sPos@(sX, sY)},
      thrownBalls
    } = do
    let d = distance slingshotCenter sPos
        initVelocityNorm = maxInitVelocity * d / slingshotRadius
        (bX, bY) = slingshotCenter
        v =
          ( initVelocityNorm * (bX - sX) / d,
            initVelocityNorm * (bY - sY) / d
          )

    let discInfo =
          DiscInfo
            { discRadius = ballRadius,
              discElasticity = 0.9,
              discFriction = 0.5,
              discMass = 5
            }
    newBall <- createBall space lambdaBall discInfo (sX, sY) v
    pure $
      world
        { slingshot = slingshot {slingshotState = Free},
          thrownBalls = newBall : thrownBalls
        }
handleEvent
  _
  (EventKey (MouseButton LeftButton) Down _ mousePos)
  world@World
    { slingshot =
        slingshot@Slingshot
          { slingshotState = Free
          , slingshotBallRadius
          , slingshotCenter
          }
    }
    | grabCircle slingshotBallRadius slingshotCenter mousePos = do
      let world' = world {slingshot = slingshot {slingshotState = Grabbed (clipSlingshotPosition slingshot mousePos)}}
      -- print $ show $ world'
      pure world'
handleEvent _ _ world = pure world

handleCollision :: Space -> World -> Collision -> IO World
handleCollision
  space
  world
  collision@Collision
    { collisionObjA =
        CollisionObject
          { objectCollisionType = EnemyCT,
            objectBody = enemyBody
          }
    } = handleEnemyCollision space world enemyBody (collision & collisionTotalImpulse)
handleCollision
  space
  world
  collision@Collision
    { collisionObjB =
        CollisionObject
          { objectCollisionType = EnemyCT,
            objectBody = enemyBody
          }
    } = handleEnemyCollision space world enemyBody (collision & collisionTotalImpulse)
handleCollision _ world _ = pure world

handleEnemyCollision :: Space -> World -> Body -> (Float, Float) -> IO World
handleEnemyCollision space world enemyBody (impulseX, impulseY) = do
  let impulse = impulseX ** 2 + impulseY ** 2
  if impulse > 100000
    then
      ( do
          let iterFunc body shape _ =
                spaceRemoveShape space shape
          bodyEachShape enemyBody iterFunc nullPtr
          spaceRemoveBody space enemyBody
          pure $
            world
              { enemies =
                  filter
                    ( \(Enemy gameObj) ->
                        objBody gameObj /= enemyBody
                    )
                    (enemies world)
              }
      )
    else pure world

grabCircle :: Radius -> Pos -> Pos -> Bool
grabCircle radius circlePos clickPos = radius >= distance circlePos clickPos

distance :: Pos -> Pos -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

advanceWorld :: Float -> World -> World
advanceWorld _ world = world
