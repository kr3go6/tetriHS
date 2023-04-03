{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConstValues
    ( fieldHeightBlk
    , fieldWidthBlk
    , blockSideSzPx
    , fieldHeightPx
    , fieldWidthPx
    , halfBlkPx
    , halfFieldWidthBlk
    , halfFieldHeightBlk
    , initialScore
    , initialSpeed
    , initTickCnt
    , displayMode
    , stepsPerSecond
    , backgroundColor
    , listOfFigures
    , range
    , emptyLine
    , emptyField
    , makeGrid
    ) where

import Types
import Graphics.Gloss

----------------------------------------
{- 
CONSTANT
VALUES
-}
----------------------------------------



fieldHeightBlk, fieldWidthBlk :: Int
fieldHeightBlk = 22
fieldWidthBlk = 12

blockSideSzPx, fieldHeightPx, fieldWidthPx :: Int
blockSideSzPx = 40 
fieldHeightPx = blockSideSzPx * fieldHeightBlk
fieldWidthPx = blockSideSzPx * fieldWidthBlk

halfBlkPx, halfFieldWidthBlk, halfFieldHeightBlk :: Int
halfBlkPx = round $ (((fromIntegral blockSideSzPx) :: Double) / 2)
halfFieldWidthBlk = round $ (((fromIntegral fieldWidthBlk) :: Double) / 2)
halfFieldHeightBlk = round $ (((fromIntegral fieldHeightBlk) :: Double) / 2)

initialScore, initialSpeed, initTickCnt :: Int
initialScore = 0
initialSpeed = 1
initTickCnt = 0

-- game display mode
displayMode :: Display
displayMode = InWindow "Tetris" (fieldWidthPx, fieldHeightPx) (10, 10) 

stepsPerSecond :: Int
stepsPerSecond = 1

backgroundColor :: Color
backgroundColor = makeColorI 30 30 30 0

listOfFigures :: [Figure]
listOfFigures = [I, J, L, O, S, T, Z]

range :: (Int, Int)
range = (0, length listOfFigures - 1)

-- create empty line
emptyLine :: FieldLine
emptyLine = [Edge] ++ (replicate (fieldWidthBlk - 2) Empty) ++ [Edge]

-- create initial field
emptyField :: Field
emptyField = [replicate fieldWidthBlk Empty] ++ 
             (replicate (fieldHeightBlk - 2) emptyLine) ++ 
             [(replicate fieldWidthBlk Edge)]

-- list of lines that make grid on field
makeGrid :: [Picture]
makeGrid = map (\path -> (Line path)) ((map (\x -> [(x, 11 * fromIntegral(blockSideSzPx)), (x, (-11) * fromIntegral(blockSideSzPx))]) 
                [x * fromIntegral(blockSideSzPx) | x <- [(-6),(-5)..6]]) ++
           (map (\y -> [(6 * fromIntegral(blockSideSzPx), y), ((-6) * fromIntegral(blockSideSzPx), y)]) 
                [y * fromIntegral(blockSideSzPx) | y <- [(-11),(-10)..11]]))