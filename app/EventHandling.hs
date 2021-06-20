{-# LANGUAGE NamedFieldPuns #-}

module EventHandling where 

import Assets
import Chiphunk.Low
import Collisions
import Constants
import Data.Function ((&))
import Graphics.Gloss.Interface.IO.Game
import Physics
import System.Exit
import World

handleEvent :: Assets -> Event -> World -> IO World
-- Quit key event
handleEvent _ (EventKey (Char 'q') Down _ _) _ = exitSuccess

-- Grabbed slingshot event
handleEvent _ (EventMotion mousePos@(mX, mY)) world@World {slingshot = slingshot@Slingshot {slingshotRadius, slingshotCenter, slingshotState = Grabbed _}} =
    pure world {slingshot = slingshot {slingshotState = Grabbed newPos}}
    where 
        -- Final ball distance from its initial position after grab
        ballDist = min distFromInitPos slingshotRadius
        -- Distance between mouse cursor and initial position
        distFromInitPos = distance slingshotCenter mousePos
        -- Unit vector between ball
        (vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
        -- New Position
        newPos = (iX + ballDist * vX, iY + ballDist * vY)
        (iX, iY) = slingshotCenter

-- Left mouse button UP event
handleEvent Assets{lambdaBall} (EventKey (MouseButton LeftButton) Up _ _)
  world@World
    { space,
      slingshot =
        slingshot@Slingshot
          { slingshotCenter,
            slingshotRadius,
            slingshotState = Grabbed sPos@(sX, sY)
          },
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
-- Left mouse button DOWN event
handleEvent
  _
  (EventKey (MouseButton LeftButton) Down _ mousePos)
  world@World
    { slingshot =
        slingshot@Slingshot
          { slingshotState = Free,
            slingshotBallRadius,
            slingshotCenter
          }
    }
    | grabCircle slingshotBallRadius slingshotCenter mousePos = do
      let world' =
            world
              { slingshot =
                  slingshot
                    { slingshotState = Grabbed (clipSlingshotPosition slingshot mousePos)
                    }
              }
      -- print $ show $ world'
      pure world'
handleEvent _ _ world = pure world

-- Collision handling
handleCollision :: Space -> World -> Collision -> IO World
-- Enemy collision
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
--Enemy collision?
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
-- No collision
handleCollision _ world _ = pure world

handleEnemyCollision :: Space -> World -> Body -> (Float, Float) -> IO World
handleEnemyCollision space world enemyBody (impulseX, impulseY) = do
  let impulse = impulseX ** 2 + impulseY ** 2
  if impulse > 100000
    then
      ( do
          let iterFunc _ shape _ = -- too many parameters TODO
                spaceRemoveShape space shape
          bodyEachShape enemyBody iterFunc nullPtr -- do you need body?
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

clipSlingshotPosition :: Slingshot -> Pos -> Pos
clipSlingshotPosition Slingshot {slingshotCenter, slingshotRadius} mousePos@(mX, mY) =
  let -- Final ball distance from its initial position after grab
      ballDist = min distFromInitPos slingshotRadius
      -- Distance between mouse cursor and initial position
      distFromInitPos = distance slingshotCenter mousePos
      -- Unit vector between ball
      (vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
      (iX, iY) = slingshotCenter
   in (iX + ballDist * vX, iY + ballDist * vY)