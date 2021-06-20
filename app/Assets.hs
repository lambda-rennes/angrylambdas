{-# LANGUAGE RecordWildCards #-}

module Assets where

import Graphics.Gloss (Picture, loadBMP, scale)

data Assets = Assets
  { landscape :: Picture,
    wood :: Picture,
    woodenLog :: Picture,
    lambdaBall :: Picture,
    monsterBind :: Picture
  }

load :: IO Assets
load = do

    wood <- loadBMP "imgs/882.bmp"
    landscape <- loadBMP "imgs/landscape5.bmp"
    woodenLog <- loadBMP "imgs/pitoune.bmp"
    lambdaBall <- scale 0.69 0.69 <$> loadBMP "imgs/lambda.bmp"
    monsterBind <- scale 0.69 0.69 <$> loadBMP "imgs/monster.bmp"

    pure Assets {..}
