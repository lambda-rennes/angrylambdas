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
import GameLoop
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Physics
import Rendering
import System.Exit
import Utils
import World
import EventHandling

main :: IO ()
main = do
  -- load images
  assets <- Assets.load

  -- Create Space
  space <- createSpace gravity

  -- Create Wolrd
  world <- createWorld assets space
  
  -- Setup Collision queue
  collisionQueue <- createCollisionQueue space
  
  -- Main function!
  gameLoop space world collisionQueue (render assets) (handleEvent assets) (flip (handleCollision space)) (\_ world -> pure world)

