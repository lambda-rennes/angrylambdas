{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (foldM)
import Data.Function ((&))
import Data.StateVar (StateVar, mapStateVar)
import Foreign.Ptr (nullPtr)
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Chiphunk.Low
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import System.Exit


data CollisionType'
  = DefaultCT
  | GroundCT
  | BlockCT
  | BallCT
  | EnemyCT
  deriving (Eq, Enum, Show)

data EnemyCollision = EnemyCollision
  { enemyCollisionBody :: Enemy
  , enemyCollisionTotalImpulse :: Vect
  }

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

spaceAddWildcardHandler' :: Space -> CollisionType' -> IO CollisionHandlerPtr
spaceAddWildcardHandler' space ct1 = spaceAddWildcardHandler space
  (toEnum $ fromEnum ct1)

window :: Display
window = InWindow "Abstract them all" (2000,1000) (500,500)

groundAX, groundAY, groundBX, groundBY :: Floating a => a
groundAX = -2500
groundAY = -250
groundBX = 2500
groundBY = -250



-- Vertices of the ground
(groundA, groundB) =
  ( Vect groundAX groundAY
  , Vect groundBX groundBY )

groundFriction = 1



initialBallPosition = Vect 0 250
ballRadius :: Floating a => a
ballRadius = 50
ballMass = 1
ballFriction = 0.7

collisionCallback :: CollisionCallback Bool
collisionCallback arbiter space _ = do
  putStrLn "COLLISION !"
  pure True


enemyCollisionCallback :: TQueue EnemyCollision -> CollisionCallback ()
enemyCollisionCallback collisionQueue arbiter space _ = do
  (enemyCollisionBody, _) <- get $ arbiterBodies arbiter
  enemyCollisionTotalImpulse <- get $ arbiterTotalImpulse arbiter
  STM.atomically $ TQueue.writeTQueue collisionQueue $ EnemyCollision {..}



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

initSlingshotPos = (-350, 200)

initBall :: Slingshot 
initBall = Slingshot ballRadius initSlingshotPos Free

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
    translate (double2Float $ vX bodyPos) (double2Float $ vY bodyPos) $
    rotate (- (rad2deg $ double2Float bodyAngle)) $
    rectangleSolid (double2Float width) (double2Float height)
    -- polygon $
    -- rectanglePath (double2Float width) (double2Float height)

renderWood :: Block -> IO Picture
renderWood block = do
  wood <- loadBMP "imgs/882.bmp"
  bodyPos <- get $ block & blockBody & bodyPosition
  bodyAngle <- get $ block & blockBody & bodyAngle
  pure $
    translate (double2Float $ vX bodyPos) (double2Float $ vY bodyPos) $
    rotate (- (rad2deg $ double2Float bodyAngle)) $
    wood

main :: IO ()
main = do

  -- Space
  let gravity = Vect 0 (-500)
  space <- createSpace gravity

  collisionQueue <- STM.atomically TQueue.newTQueue
  world <- createWorld space collisionQueue

  playIO window black 60 world render handleEvent (advanceSim space collisionQueue advanceWorld)


maxGrabDist = 300.0

ballInitPos :: Pos
ballInitPos = initSlingshotPos

maxInitVelocity :: Floating a => a
maxInitVelocity = 2500

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

      pure world{slingshot = ball{slingshotPosition = newPos}}


handleEvent (EventKey (Char 'q') Down _ _) _ = exitSuccess

handleEvent (EventKey (MouseButton LeftButton) Up _ _) 
            world@World{space, slingshot = (ball@Slingshot{slingshotPosition = sPos@(sX, sY), slingshotGrabbed = Grabbed }), thrownBalls} = do
              let d = distance ballInitPos sPos
                  initVelocityNorm = maxInitVelocity * d / maxGrabDist
                  (bX, bY) = ballInitPos
                  v = Vect
                    (float2Double $ initVelocityNorm * (bX-sX) / d)
                    (float2Double $ initVelocityNorm * (bY-sY) / d)


              newBall <- createBall space ballRadius (Vect (float2Double sX) (float2Double sY)) v
              pure $
                world
                  { slingshot = initBall
                  , thrownBalls = newBall : thrownBalls
                  } 

handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos)
            world@World{slingshot = (ball@Slingshot{slingshotRadius, slingshotPosition, slingshotGrabbed = Free}) } 
            | grabCircle slingshotRadius slingshotPosition mousePos == True = do 
              let world' = world{slingshot = ball{slingshotGrabbed = Grabbed}}
              -- print $ show $ world'
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
           -> TQueue EnemyCollision
           -> (Float -> World -> World) 
           -> Float -> World -> IO World
advanceSim space collisionQueue advance tic world = do
  collisions <- STM.atomically $ TQueue.flushTQueue collisionQueue
  let handleCollision world EnemyCollision{enemyCollisionBody, enemyCollisionTotalImpulse} = do
        let impulse = (vX enemyCollisionTotalImpulse) ** 2 + (vY enemyCollisionTotalImpulse) ** 2
        case impulse > 100000 of
          True -> do
            let iterFunc body shape _ =
                  spaceRemoveShape space shape
            bodyEachShape enemyCollisionBody iterFunc nullPtr
            spaceRemoveBody space enemyCollisionBody
            pure $ world { enemies = filter (/= enemyCollisionBody) (enemies world) }
          False ->
            pure world

  world' <- foldM handleCollision world collisions
  spaceStep space (1 / 60)
  pure $ advance tic world'



render :: World -> IO Picture
render World{blocks, slingshot, log', thrownBalls, enemies} = do
  landscape <- loadBMP "imgs/landscape3.bmp"

  let getPosAngle body = (,) <$> get (bodyPosition body) <*> get (bodyAngle body)
  ballPosAngles <- traverse getPosAngle thrownBalls
  ballPictures <- flip traverse ballPosAngles $ \(Vect x y, angle) ->
    translate (double2Float x) (double2Float y) . rotate (- (double2Float $ rad2deg angle)) <$> renderLambda (0, 0)


  enemyPosAngles <- traverse getPosAngle enemies
  let enemyPictures = flip fmap enemyPosAngles $ \(Vect x y, angle) ->
        color yellow .
        translate (double2Float x) (double2Float y) $ circleSolid ballRadius

  blockPictures <- traverse renderWood blocks
  lambda <- renderLambda (-400.0, -200.0)

  slingshot <- renderSlingshot slingshot
  pure $ mconcat $ [landscape] <>
    [groundPicture] <> [renderLog log'] <>
    ballPictures <>
    enemyPictures <>
    blockPictures <> [slingshot]

leftBankAX, leftBankAY, leftBankBX, leftBankBY :: Floating a => a 
leftBankAX = -1000
leftBankAY = -220
leftBankBX = -310
leftBankBY = -175

(leftGroundA, leftGroundB) = 
  ( Vect leftBankAX leftBankAY
  , Vect leftBankBX leftBankBY ) 

rightBankAX, rightBankAY, rightBankBX, rightBankBY :: Floating a => a 
rightBankAX = 225
rightBankAY = -150
rightBankBX = 1000
rightBankBY = -100

(rightGroundA, rightGroundB) = 
  ( Vect rightBankAX rightBankAY
  , Vect rightBankBX rightBankBY ) 

groundPicture :: Picture
groundPicture = 
  pictures [ color yellow $ line [(leftBankAX, leftBankAY), (leftBankBX, leftBankBY)]
           , color orange $ line [(rightBankAX, rightBankAY), (rightBankBX, rightBankBY)]
           ]

renderSlingString :: Pos -> Pos -> Picture
renderSlingString (x1, y1) (x2, y2)= color blue $ Line [(x1, y1), (x2, y2)]

renderSlingBall :: Radius -> Pos -> Picture
renderSlingBall radius (x, y) = translate x y $ color yellow $ circleSolid radius

renderSlingshot :: Slingshot -> IO Picture
renderSlingshot (Slingshot radius pos _) = do 
  lambdaBall <- renderLambda pos 
  pure $ pictures [ renderSlingString initSlingshotPos pos
                  , lambdaBall
                  ]


data Log = 
  Log  { logPicture :: Picture
        , logBody :: Body
        , logDimension :: Vect
        } deriving Show

logX, logY :: Floating a => a 
logX = 0
logY = -145

renderLog :: Log -> Picture
renderLog log = logPicture log 

createLog :: Space -> Pos -> IO Log
createLog space pos = do
  logImg <- loadBMP "imgs/pitoune.bmp"

  logBody <- bodyNew logMass logMoment
  spaceAddBody space logBody
  let Vect width height = logDimension
  logShape <- boxShapeNew logBody width height 0
  spaceAddShape space logShape

  shapeFriction logShape $= 0.5
  shapeElasticity logShape $= 0.8

  bodyPosition logBody $= pos2Vect pos
  bodyAngle logBody $= 0

  pure $ Log logImg logBody logDimension

  where
    logMass = 0.5
    logMoment = momentForBox logMass
      (logDimension & vX)
      (logDimension & vY)
    logRadius = 2
    logDimension = Vect 700 200


renderLambda :: Pos -> IO Picture
renderLambda (x, y) = do
  lambda <- loadBMP "imgs/lambda.bmp"
  pure $ translate x y $ scale 0.69 0.69 $ lambda

data World =
  World { space :: Space
        , blocks :: [Block]
        , slingshot :: Slingshot
        , log' :: Log
        , thrownBalls :: [Ball]
        , enemies :: [Enemy]
        } deriving Show

createWorld :: Space -> TQueue EnemyCollision -> IO World
createWorld space collisionQueue = do

    -- Ground
    _ <- createGround space 
    
    -- Log

    logObj <- createLog space (logX, logY)

    -- Blocks
    blocks <- traverse (createBlock space) blockDescriptions
    

    -- Ball 
    -- ballBody <- createBall space ballRadius (Vect (-100) 300) (Vect 200 0)
    enemy <- createEnemy space ballRadius (Vect 300 400) (Vect 0 0)

    _ <- createCallBacks space collisionQueue


    -- logImg <- lambda <- loadBMP "imgs/lambda.bmp" 

    -- World
    pure $ World space blocks initBall logObj [] [enemy]

type Ground = Shape

createGround :: Space -> IO ()
createGround space = do
  spaceBody <- get $ spaceStaticBody space
  leftGround <- segmentShapeNew spaceBody leftGroundA leftGroundB 0

  shapeFriction leftGround $= groundFriction
  shapeElasticity leftGround $= 0.9
  groundCollisionType <- get $ shapeCollisionType leftGround

  spaceAddShape space leftGround
  shapeCollisionType' leftGround $= GroundCT

  rightGround <- segmentShapeNew spaceBody rightGroundA rightGroundB 0
  shapeFriction rightGround $= groundFriction
  shapeElasticity rightGround $= 0.9
  groundCollisionType <- get $ shapeCollisionType rightGround

  spaceAddShape space rightGround
  shapeCollisionType' rightGround $= GroundCT

  pure ()

type Gravity = Vect

createSpace :: Gravity -> IO Space 
createSpace gravity = do 
  space <- spaceNew
  spaceGravity space $= gravity
  pure space

blockDescriptions :: [BlockDescription]
blockDescriptions =
   [ BlockDescription
      { bdescPosition = Vect 250 (-25)
      , bdescDimensions = Vect 25 250
      , bdescAngle = 0
      }
    , BlockDescription
        { bdescPosition = Vect 350 (-25)
        , bdescDimensions = Vect 25 250
        , bdescAngle = 0
        }
    , BlockDescription
      { bdescPosition = Vect 300 (70)
      , bdescDimensions = Vect 25 250
      , bdescAngle = 3.1415/2
      }
    ]

type Ball = Body
type Enemy = Body

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

createEnemy :: Space -> Double -> Vect -> Vect -> IO Ball
createEnemy space radius initPos initVelocity = do
  let moment = momentForCircle ballMass 0 radius (Vect 0 0)

  enemyBody <- bodyNew ballMass moment
  spaceAddBody space enemyBody
  bodyPosition enemyBody $= initPos
  bodyVelocity enemyBody $= initVelocity

  enemyShape <- circleShapeNew enemyBody radius (Vect 0 0)

  shapeFriction enemyShape $= 0.7
  shapeElasticity enemyShape $= 0

  shapeCollisionType' enemyShape $= EnemyCT

  spaceAddShape space enemyShape

  pure enemyBody

createCallBacks :: Space -> TQueue EnemyCollision -> IO ()
createCallBacks space collisionQueue = do
  callback <- mkCallback (enemyCollisionCallback collisionQueue)
  colHandlerPtr <- spaceAddWildcardHandler' space EnemyCT
  modifyCollisionHandler colHandlerPtr $ \colHandler -> pure $ colHandler { chPostSolveFunc = callback }

  pure ()
