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
handleEvent _ (EventKey (MouseButton LeftButton) Up _ _) world = do
  print $ slingshotCenter . slingshot $ world
  putStrLn "Slingshot clicked!"
  return world
handleEvent _ _ world = pure world

handleCollision :: Space -> World -> Collision -> IO World
handleCollision _ world _ = pure world