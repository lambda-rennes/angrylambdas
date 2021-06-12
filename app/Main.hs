{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Function ((&))
import Data.StateVar (StateVar, mapStateVar)
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Chiphunk.Low

data CollisionType'
  = GroundCT
  | BlockCT
  | BallCT
  deriving (Eq, Enum, Show)

shapeCollisionType' :: Shape -> StateVar CollisionType'
shapeCollisionType' =
  mapStateVar
    (toEnum . fromEnum)
    (toEnum . fromEnum)
  . shapeCollisionType

spaceAddCollisionHandler' :: Space -> CollisionType' -> CollisionType' -> IO CollisionHandlerPtr
spaceAddCollisionHandler' space ct1 ct2 = spaceAddCollisionHandler space
  (toEnum $ fromEnum ct1)
  (toEnum $ fromEnum ct2)

window :: Display
window = InWindow "Abstract them all" (2000,1000) (500,500)

groundAX, groundAY, groundBX, groundBY :: Floating a => a
groundAX = -500
groundAY = 5
groundBX = 500
groundBY = 5

-- Vertices of the ground
(groundA, groundB) =
  ( Vect groundAX groundAY
  , Vect groundBX groundBY )

groundFriction = 1

initialBallPosition = Vect 0 50
ballRadius = 5
ballMass = 1
ballFriction = 0.7

collisionCallback :: CollisionCallback Bool
collisionCallback arbiter space _ = do
  putStrLn "COLLISION !"
  pure True


data BlockDescription =
  BlockDescription
    { bdescPosition :: Vect
    , bdescDimensions :: Vect
    , bdescAngle :: Double
    }

data Block =
  Block
   { blockDimensions :: Vect
   , blockBody :: Body
   }


-- data Object a =
--   Object
--     { data :: a
--     ,  :: Body
--     }

createBlock :: Space -> BlockDescription -> IO Block
createBlock space bdesc = do
    blockBody <- bodyNew blockMass blockMoment
    spaceAddBody space blockBody
    let Vect width height = bdescDimensions bdesc
    blockShape <- boxShapeNew blockBody width height 0
    spaceAddShape space blockShape

    shapeFriction blockShape $= 0.5
    shapeElasticity blockShape $= 0.8

    bodyPosition blockBody $= bdescPosition bdesc
    bodyAngle blockBody $= bdescAngle bdesc
    pure $ Block
      { blockBody = blockBody
      , blockDimensions = bdescDimensions bdesc
      }
  where
    blockMass = 0.5
    blockMoment = momentForBox blockMass
      (bdesc & bdescDimensions & vX)
      (bdesc & bdescDimensions & vY)
    blockRadius = 2

rad2deg x = x*180 / 3.1415

renderBlock :: Block -> IO Picture
renderBlock block = do
  let Vect width height = block & blockDimensions
  bodyPos <- get $ block & blockBody & bodyPosition
  bodyAngle <- get $ block & blockBody & bodyAngle
  -- pure $
  --   translate (double2Float $ vX bodyPos) (double2Float $ vY bodyPos) $ color red $ circleSolid 5

  pure $
    color white $
    translate (double2Float $ vX bodyPos) (double2Float $ vY bodyPos) $
    rotate (rad2deg $ double2Float bodyAngle) $
    rectangleSolid (double2Float width) (double2Float height)
    -- polygon $
    -- rectanglePath (double2Float width) (double2Float height)

main :: IO ()
main = do

  -- Space
  let gravity = Vect 0 (-100)
  space <- createSpace gravity
  
  world <- createWorld space

  simulateIO window black 60 world render (advanceSim space)

advanceSim :: Space -> ViewPort -> Float -> World -> IO World
advanceSim space _ _ world = do 
  spaceStep space (1 / 60)
  pure world

render :: World -> IO Picture
render World{blocks, ball} = do
  pos <- get $ bodyPosition ball
  blockPictures <- traverse renderBlock blocks
  pure $ scale 5 5 $ mconcat $
    [ translate (double2Float $ vX pos) (double2Float $ vY pos) $ color red $ circleSolid 5
    , color yellow $ line [(groundAX, groundAY), (groundBX, groundBY)]
    ] <>
    blockPictures


data World =
  World { blocks :: [Block]
        , ball :: Ball
        }

createWorld :: Space -> IO World
createWorld space = do

    -- Ground
    _ <- createGround space 
    
    -- Blocks
    blocks <- traverse (createBlock space) blockDescriptions
    
    -- Ball 
    ballBody <- createBall space

    -- Call backs
    _ <- createCallBacks space

    -- World
    pure $ World blocks ballBody

type Ground = Shape

createGround :: Space -> IO Ground
createGround space = do
  spaceBody <- get $ spaceStaticBody space
  ground <- segmentShapeNew spaceBody groundA groundB 0

  shapeFriction ground $= groundFriction
  shapeElasticity ground $= 0.9
  groundCollisionType <- get $ shapeCollisionType ground

  spaceAddShape space ground
  shapeCollisionType' ground $= GroundCT

  pure ground

type Gravity = Vect

createSpace :: Gravity -> IO Space 
createSpace gravity = do 
  space <- spaceNew
  spaceGravity space $= gravity
  pure space

blockDescriptions :: [BlockDescription]
blockDescriptions =
   [ BlockDescription
      { bdescPosition = Vect 50 25
      , bdescDimensions = Vect 5 50
      , bdescAngle = 0
      }
    , BlockDescription
        { bdescPosition = Vect 70 25
        , bdescDimensions = Vect 5 50
        , bdescAngle = 0
        }
    , BlockDescription
      { bdescPosition = Vect 60 55
      , bdescDimensions = Vect 5 50
      , bdescAngle = 3.1415/2
      }
    ]

type Ball = Body

createBall :: Space -> IO Ball
createBall space = do
  let moment = momentForCircle ballMass 0 ballRadius (Vect 0 0)

  ballBody <- bodyNew ballMass moment
  spaceAddBody space ballBody
  bodyPosition ballBody $= initialBallPosition
  bodyVelocity ballBody $= Vect 50 0

  ballShape <- circleShapeNew ballBody ballRadius (Vect 0 0)

  shapeFriction ballShape $= ballFriction
  shapeElasticity ballShape $= 0.9

  shapeCollisionType' ballShape $= BallCT
  

  spaceAddShape space ballShape

  pure ballBody

createCallBacks :: Space -> IO ()
createCallBacks space = do
  callback <- mkCallbackB collisionCallback
  colHandlerPtr <- spaceAddCollisionHandler' space GroundCT BallCT
  modifyCollisionHandler colHandlerPtr $ \colHandler -> pure $ colHandler { chBeginFunc = callback }
  pure ()
