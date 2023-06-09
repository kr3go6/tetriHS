{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConstValues
    ( fieldHeightBlk
    , fieldWidthBlk
    , blockSideSzPx
    , fieldHeightPx
    , fieldWidthPx
    , appHeightPx
    , appWidthPx
    , infoWidthPx
    , halfInfoWidthPx
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
    , figureToField
    , earnedScores
    , scoreToSpeed
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

appHeightPx, appWidthPx :: Int
appHeightPx = fieldHeightPx
appWidthPx = round $ ((fromIntegral fieldWidthPx) :: Double) * 2

infoWidthPx :: Int
infoWidthPx = appWidthPx - fieldWidthPx

halfInfoWidthPx :: Int
halfInfoWidthPx = round $ ((fromIntegral infoWidthPx) :: Double) / 2

halfBlkPx, halfFieldWidthBlk, halfFieldHeightBlk :: Int
halfBlkPx = round $ ((fromIntegral blockSideSzPx) :: Double) / 2
halfFieldWidthBlk = round $ ((fromIntegral fieldWidthBlk) :: Double) / 2
halfFieldHeightBlk = round $ ((fromIntegral fieldHeightBlk) :: Double) / 2

initialScore, initialSpeed, initTickCnt :: Int
initialScore = 0
initialSpeed = 30
initTickCnt = 0

-- game display mode
displayMode :: Display
displayMode = InWindow "Tetris" (appWidthPx, appHeightPx) (10, 10) 

stepsPerSecond :: Int
stepsPerSecond = 30

backgroundColor :: Color
backgroundColor = makeColorI 30 30 30 0

listOfFigures :: [Figure]
listOfFigures = [I, J, L, O, S, T, Z]

range :: (Int, Int)
range = (0, length listOfFigures - 1)

-- how many points do player get when he removes lines
earnedScores :: Int -> Int
earnedScores 1 = 100
earnedScores 2 = 250
earnedScores 3 = 450
earnedScores 4 = 700
earnedScores _ = 0

scoreToSpeed :: Int -> Int
scoreToSpeed x  | x < 2000   = 30
                | x < 3000   = 20
                | x < 4000   = 15
                | x < 5000   = 10
                | x < 8000   = 5
                | x < 12000  = 3
                | x < 15000  = 2
                | otherwise  = 1

-- create empty line
emptyLine :: FieldLine
emptyLine = [Edge] ++ (replicate (fieldWidthBlk - 2) Empty) ++ [Edge]

-- create initial field
emptyField :: Field
emptyField = [[InvisibleEdge] ++ (replicate (fieldWidthBlk - 2) Empty) ++ [InvisibleEdge]] ++ 
             (replicate (fieldHeightBlk - 2) emptyLine) ++ 
             [(replicate fieldWidthBlk Edge)]

-- list of lines that make grid on field
makeGrid :: [Picture]
makeGrid  = map 
                (\path -> 
                    (Translate (-(fromIntegral halfInfoWidthPx)) 0 $ Line path)
                ) 
                (
                    (map 
                        (\x -> 
                            [(x, 11 * fromIntegral(blockSideSzPx)), 
                            (x, (-11) * fromIntegral(blockSideSzPx))]
                        ) 
                        [x * fromIntegral(blockSideSzPx) | x <- [(-6),(-5)..6]]
                    ) ++
                    (map 
                        (\y -> 
                            [(6 * fromIntegral(blockSideSzPx), y), 
                            ((-6) * fromIntegral(blockSideSzPx), y)]
                        ) 
                        [y * fromIntegral(blockSideSzPx) | y <- [(-11),(-10)..11]]
                    )
                )

-- matrix representation of a figure
figureToField :: Figure -> Field
figureToField O = [[Yellow, Yellow],
                   [Yellow, Yellow]]
figureToField T = [[Empty, Purple, Empty],
                   [Purple, Purple, Purple]]
figureToField I = [[Cyan],
                   [Cyan],
                   [Cyan],
                   [Cyan]]
figureToField J = [[Blue, Blue],
                   [Blue, Empty],
                   [Blue, Empty]]
figureToField L = [[Orange, Orange],
                   [Empty, Orange],
                   [Empty, Orange]]
figureToField S = [[Green, Empty],
                   [Green, Green],
                   [Empty, Green]]
figureToField Z = [[Empty, Red],
                   [Red, Red],
                   [Red, Empty]]
figureToField None = [[]]