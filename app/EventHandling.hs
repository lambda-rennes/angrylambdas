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
handleEvent _ (EventKey (MouseButton LeftButton) Down _ position) world@World{slingshot} =
  pure $ if clickedSlingshot then
    world { slingshot = slingshot { slingshotState = Grabbed position } }
    -- do
    --   print slingshotCenter
    --   putStrLn "Slingshot clicked!"
    --   case slingshotState of
    --     Free -> pure world{slingshot = slingshot{slingshotState = Grabbed position}}
    --     Grabbed oldPosition -> do
    --       putStrLn $ "Slingshot released, NIY (" ++ show oldPosition ++ ")"
    --       pure world
  else
    world
  where
    Slingshot{slingshotBallRadius, slingshotCenter} = slingshot 
    clickedSlingshot = slingshotBallRadius >= distance slingshotCenter position

handleEvent _ (EventKey (MouseButton LeftButton) Up _ position) world@World{slingshot} =
  pure $ world { slingshot = slingshot { slingshotState = Free } }

handleEvent _ (EventMotion position) world@World{slingshot} =
  case slingshotState slingshot of
    Free -> pure world
    Grabbed _ -> pure $ world { slingshot = slingshot { slingshotState = Grabbed position } }


handleEvent _ _ world = pure world

handleCollision :: Space -> World -> Collision -> IO World
handleCollision _ world _ = pure world


