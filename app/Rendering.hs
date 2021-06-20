{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Rendering where

import Assets
import World

import Data.Function ((&))

import Utils
import Graphics.Gloss

import Chiphunk.Low
import Constants

render :: Assets -> World -> IO Picture
render assets@Assets {..} World {slingshot, log', thrownBalls, enemies} = do
  
  ballPictures <- traverse renderBall thrownBalls
  enemyPictures <- traverse renderEnemy enemies
  slingshotPicture <- renderSlingshot assets slingshot
  logPicture <- renderLog log'

  pure $
    mconcat $
      [landscape]
        <> [groundPicture]
        <> [logPicture]
        <> ballPictures
        <> enemyPictures
        <> [slingshotPicture]

renderEnemy :: Enemy -> IO Picture
renderEnemy (Enemy gameObject) = renderGameObject gameObject

renderLog :: Log -> IO Picture
renderLog (Log gameObject) = renderGameObject gameObject

renderSlingString :: Pos -> Pos -> Picture
renderSlingString (x1, y1) (x2, y2) = color blue $ Line [(x1, y1), (x2, y2)]

renderSlingBall :: Radius -> Pos -> Picture
renderSlingBall radius (x, y) = translate x y $ color yellow $ circleSolid radius

renderSlingshot :: Assets -> Slingshot -> IO Picture
renderSlingshot Assets{lambdaBall} Slingshot{slingshotState = Free, slingshotCenter = (x,y)} =
  pure $ translate x y lambdaBall

renderSlingshot Assets{lambdaBall} Slingshot{slingshotCenter, slingshotState = Grabbed pos@(x,y)} = do
  pure $
    pictures
      [ renderSlingString slingshotCenter pos,
        translate x y lambdaBall
      ]


renderBall :: Ball -> IO Picture
renderBall (Ball gameObject) = do
  renderGameObject gameObject

renderBlock :: Block -> IO Picture
renderBlock (Block gameObject) =do
  renderGameObject gameObject

renderGameObject :: GameObject -> IO Picture
renderGameObject GameObject{objBody, objPicture} = 
  genericRender objBody objPicture 

genericRender :: Body -> Picture -> IO Picture
genericRender body picture = do
  bodyPos <- get $ body & bodyPosition
  bodyAngle' <- get $ body & bodyAngle
  pure $
    translate (double2Float $ vX bodyPos) (double2Float $ vY bodyPos) $
      rotate (- (rad2deg $ double2Float bodyAngle')) picture


groundPicture :: Picture
groundPicture =
  pictures
    [ color yellow $ line [(leftBankAX, leftBankAY), (leftBankBX, leftBankBY)],
      color orange $ line [(rightBankAX, rightBankAY), (rightBankBX, rightBankBY)]
    ]