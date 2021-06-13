{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Function ((&))
import Data.StateVar (StateVar, mapStateVar)
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Chiphunk.Low

data CollisionType'
  = DefaultCT
  | GroundCT
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
groundAX = -2500
groundAY = 25
groundBX = 2500
groundBY = 25

-- Vertices of the ground
(groundA, groundB) =
  ( Vect groundAX groundAY
  , Vect groundBX groundBY )

groundFriction = 1

initialBallPosition = Vect 0 250
ballRadius :: Floating a => a
ballRadius = 25
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
   } deriving Show

instance Show Body where
  show _ = "Body..."

type Pos = (Float, Float)

data Grabbed = Grabbed Pos | Free deriving Show

data Ball' = 
  Ball' 
   { ballRadius' :: Float
   , ballPosition' :: Pos
   , ballGrabbed :: Grabbed
   } deriving Show

initBall :: Ball' 
initBall = Ball' 25.0 (0.0, 0.0) Free

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
  let gravity = Vect 0 (-500)
  space <- createSpace gravity
  
  world <- createWorld space

  playIO window black 60 world render handleEvent (advanceSim space advanceWorld)


handleEvent :: Event -> World -> IO World
handleEvent (EventMotion mousePos) world@World{ball' = (ball@Ball'{ballGrabbed = (Grabbed ballInitPos)}) }  | distance ballInitPos mousePos <= 300.0
  = do
      print $ "EventMotion" <> show mousePos 
      pure world{ball' = ball{ballPosition' = mousePos}}
handleEvent (EventKey (MouseButton LeftButton) Up _ _) 
            world@World{ball' = (ball@Ball'{ballGrabbed = (Grabbed _)}) } = pure $ world{ball' = ball{ballGrabbed = Free}}  
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos)
            world@World{ball' = (ball@Ball'{ballRadius', ballPosition', ballGrabbed = Free}) } 
            | grabCircle ballRadius' ballPosition' mousePos == True = do 
              let world' = world{ball' = ball{ballGrabbed = Grabbed mousePos}}
              print $ show $ world'
              pure world'
handleEvent _ world = pure world

type Radius = Float

grabCircle :: Radius -> Pos -> Pos -> Bool
grabCircle radius circlePos clickPos = radius >= distance circlePos clickPos   

distance :: Pos -> Pos -> Float
distance (x1, y1) (x2, y2) = sqrt $ ( (x2 - x1) ** 2 ) + ( (y2 - y1) ** 2)  

advanceWorld :: Float -> World -> World
advanceWorld _ world = world

advanceSim :: Space 
           -> (Float -> World -> World) 
           -> Float -> World -> IO World
advanceSim space advance tic world = do 
  spaceStep space (1 / 60)
  pure $ advance tic world

render :: World -> IO Picture
render World{blocks, ball, ball'} = do
  pos <- get $ bodyPosition ball
  blockPictures <- traverse renderBlock blocks
  pure $ mconcat $
    [ translate (double2Float $ vX pos) (double2Float $ vY pos) $ color red $ circleSolid ballRadius
    , color yellow $ line [(groundAX, groundAY), (groundBX, groundBY)]
    ] <>
    blockPictures <> [(renderBall ball')]


renderBall :: Ball' -> Picture
renderBall (Ball' radius' (x, y) _) = translate x y $ color yellow $ circleSolid radius'

data World =
  World { blocks :: [Block]
        , ball :: Ball
        , ball' :: Ball'
        } deriving Show

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
    pure $ World blocks ballBody initBall

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
      { bdescPosition = Vect 250 125
      , bdescDimensions = Vect 25 250
      , bdescAngle = 0
      }
    , BlockDescription
        { bdescPosition = Vect 350 125
        , bdescDimensions = Vect 25 250
        , bdescAngle = 0
        }
    , BlockDescription
      { bdescPosition = Vect 300 275
      , bdescDimensions = Vect 25 250
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
