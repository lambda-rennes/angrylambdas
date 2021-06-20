module GameLoop where

import Chiphunk.Low
import Graphics.Gloss.Interface.IO.Game
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue

import Control.Monad (foldM)

import World 
import Collisions

gameLoop ::
  Space ->
  World ->
  TQueue Collision ->
  (World -> IO Picture) ->
  (Event -> World -> IO World) ->
  (Collision -> World -> IO World) ->
  (Float -> World -> IO World) ->
  IO ()
gameLoop s initialWorld collisionQueue render processEvent processCollision _ = -- TODO why unused
  playIO display black 60 initialWorld render processEvent advance'
  where
    advance' _ world = do
      spaceStep s (1 / 60)
      collisions <- STM.atomically $ TQueue.flushTQueue collisionQueue
      foldM (flip processCollision) world collisions


display :: Display
display = InWindow "AngryLambdas" (1920, 1000) (0,0)
