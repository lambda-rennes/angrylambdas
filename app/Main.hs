{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Assets
import Collisions
import Constants
import GameLoop
import Rendering
import World
import EventHandling

main :: IO ()
main = do
  -- load images
  assets <- Assets.load

  -- Create Space
  space <- createSpace gravity

  -- Create World
  world <- createWorld assets space
  
  -- Setup Collision queue
  collisionQueue <- createCollisionQueue space
  
  -- Main function!
  gameLoop space world collisionQueue (render assets) (handleEvent assets) (flip (handleCollision space)) (\_ w -> pure w)

