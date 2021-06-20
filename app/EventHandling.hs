{-# LANGUAGE NamedFieldPuns #-}

module EventHandling where

import Assets
import Chiphunk.Low
import Collisions
import Constants
import Data.Function ((&))
import Graphics.Gloss.Interface.IO.Game
    ( Key(MouseButton, Char),
      KeyState(Down, Up),
      MouseButton(LeftButton),
      Event(EventKey, EventMotion) )
import Physics
import System.Exit
import World

handleEvent :: Assets -> Event -> World -> IO World
-- Quit key event
handleEvent _ (EventKey (Char 'q') Down _ _) _ = exitSuccess

-- Moving slingshot around
handleEvent _ (EventMotion mousePos@(mX, mY)) world@World {slingshot = Slingshot {slingshotState = Grabbed _}} =
    pure world {slingshot = sling  {slingshotState = Grabbed newPos}}
    where
        -- Final ball distance from its initial position after grab
        ballDist = min distFromInitPos slingshotRadius
        -- Distance between mouse cursor and initial position
        distFromInitPos = distance slingshotCenter mousePos
        -- Unit vector between ball
        (vX, vY) = ((mX - iX) / distFromInitPos, (mY - iY) / distFromInitPos)
        -- New Position
        newPos = (iX + ballDist * vX, iY + ballDist * vY)
        sling@Slingshot {slingshotRadius, slingshotCenter} = slingshot world
        (iX, iY) = slingshotCenter

handleEvent Assets{lambdaBall} (EventKey (MouseButton LeftButton) Up _ _) world@World{slingshot} =
  case slingshotState slingshot of
    Grabbed sPos@(sX, sY) -> do
      let d = distance (slingshotCenter slingshot) sPos
          initVelocityNorm = maxInitVelocity * d / slingshotRadius slingshot
          (bX, bY) = slingshotCenter slingshot
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
      newBall <- createBall (space world) lambdaBall discInfo (sX, sY) v
      pure $
        world
          { slingshot = slingshot {slingshotState = Free},
            thrownBalls = newBall : thrownBalls world
          }
    _ -> pure world


  -- Left mouse button DOWN event
handleEvent
  _
  (EventKey (MouseButton LeftButton) Down _ mousePos)
  world@World{slingshot} = do
    let Slingshot {slingshotCenter, slingshotRadius, slingshotBallRadius} = slingshot

    if distance slingshotCenter mousePos < slingshotBallRadius then (
      let grabPos = clipSlingshotPosition slingshot mousePos in
      pure $ world { slingshot = slingshot { slingshotState = Grabbed grabPos}}) else pure world
  
handleEvent _ _ world = pure world

handleCollision :: Space -> World -> Collision -> IO World
handleCollision space world collision =
    handleObject (collision & collisionObjA) world >>=
    handleObject (collision & collisionObjB)
  where
    handleObject CollisionObject {objectCollisionType = EnemyCT, objectBody} w =
      handleEnemyCollision space w objectBody (collision & collisionTotalImpulse)
    handleObject _ w = pure w

handleEnemyCollision :: Space -> World -> Body -> (Float, Float) -> IO World
handleEnemyCollision space world enemyBody (impulseX, impulseY) = do
  let impulse = impulseX ** 2 + impulseY ** 2
  if impulse > 100000
    then
      ( do
          let iterFunc _ shape _ =
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