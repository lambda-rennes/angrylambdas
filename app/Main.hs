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

instance Show Space where
  show _ = "Space.."

type Pos = (Float, Float)

data Grabbed = Grabbed | Free deriving Show

data Slingshot = 
  Slingshot 
   { slingshotRadius :: Float
   , slingshotPosition :: Pos
   , slingshotGrabbed :: Grabbed
   } deriving Show

initBall :: Slingshot 
initBall = Slingshot 25.0 (0.0, 0.0) Free

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


maxGrabDist = 300.0

ballInitPos :: Pos
ballInitPos = (0, 0)

handleEvent :: Event -> World -> IO World
handleEvent (EventMotion mousePos@(mX, mY)) world@World{slingshot = (ball@Slingshot{slingshotGrabbed = Grabbed }) } 
  = do
      let -- Final ball distance from its initial position after grab
          ballDist = min distFromInitPos maxGrabDist
          -- Distance between mouse cursor and initial position
          distFromInitPos = distance ballInitPos mousePos
          -- Unit vector between ball
          v@(vX, vY) = ((mX-iX)/distFromInitPos, (mY-iY)/distFromInitPos)
          -- New Position
          newPos = (iX + ballDist * vX, iY + ballDist * vY)
          (iX, iY) = ballInitPos

      print $ "EventMotion" <> show mousePos 
      pure world{slingshot = ball{slingshotPosition = newPos}}

handleEvent (EventKey (MouseButton LeftButton) Up _ _) 
            world@World{space, slingshot = (ball@Slingshot{slingshotGrabbed = Grabbed }), thrownBalls} = do
              newBall <- createBall space ballRadius (Vect (-100) 300) (Vect 200 0)

              pure $
                world
                  { slingshot = ball{slingshotGrabbed = Free}
                  , thrownBalls = newBall : thrownBalls
                  } 

handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos)
            world@World{slingshot = (ball@Slingshot{slingshotRadius, slingshotPosition, slingshotGrabbed = Free}) } 
            | grabCircle slingshotRadius slingshotPosition mousePos == True = do 
              let world' = world{slingshot = ball{slingshotGrabbed = Grabbed}}
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
render World{blocks, slingshot, thrownBalls} = do
  thrownBallPositions <- traverse (get . bodyPosition) thrownBalls
  let ballPictures = flip fmap thrownBallPositions $ \(Vect x y) ->
        translate (double2Float x) (double2Float y) $ color red $ circleSolid ballRadius
  blockPictures <- traverse renderBlock blocks
  pure $ mconcat $
    [ color yellow $ line [(groundAX, groundAY), (groundBX, groundBY)]
    ] <>
    ballPictures <>
    blockPictures <> [(renderBall slingshot)]


renderBall :: Slingshot -> Picture
renderBall (Slingshot radius' (x, y) _) = translate x y $ color yellow $ circleSolid radius'

data World =
  World { space :: Space
        , blocks :: [Block]
        , slingshot :: Slingshot
        , thrownBalls :: [Ball]
        } deriving Show

createWorld :: Space -> IO World
createWorld space = do

    -- Ground
    _ <- createGround space 
    
    -- Blocks
    blocks <- traverse (createBlock space) blockDescriptions
    
    -- Ball 
    ballBody <- createBall space ballRadius (Vect (-100) 300) (Vect 200 0)

    -- Call backs
    _ <- createCallBacks space

    -- World
    pure $ World space blocks initBall [ballBody]

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

vect2Pos :: Vect -> Pos
vect2Pos (Vect x y) = (double2Float x, double2Float y)
pos2Vect :: Pos -> Vect
pos2Vect (x, y) = Vect (float2Double x) (float2Double y)

createBall :: Space -> Double -> Vect -> Vect -> IO Ball
createBall space radius initPos initVelocity = do
  let moment = momentForCircle ballMass 0 radius (Vect 0 0)

  ballBody <- bodyNew ballMass moment
  spaceAddBody space ballBody
  bodyPosition ballBody $= initPos
  bodyVelocity ballBody $= initVelocity

  ballShape <- circleShapeNew ballBody radius (Vect 0 0)

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
