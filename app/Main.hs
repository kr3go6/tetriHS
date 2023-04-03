{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Color
import System.Random

import Types
import ConstValues
import GameLogic


-- display all objects
displayField :: State -> Picture
displayField state@(State {..}) = Pictures $ (displayField_auc (addFigToField fld (figureToField fig) x y alpha) 0 0) ++ makeGrid

displayField_auc :: Field -> Xcoord -> Ycoord -> [Picture]
displayField_auc fld x y | (x == (fieldWidthBlk - 1)) && 
                            (y == (fieldHeightBlk - 1)) 
                                    = [(Translate (fromIntegral (blockSideSzPx * halfFieldWidthBlk - halfBlkPx))
                                       (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                       $ displayBlock ((fld !! y) !! x)
                                       $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                         | (x >= fieldWidthBlk)
                                    = displayField_auc fld 0 (y + 1)
                         | otherwise
                                    =  [(Translate (fromIntegral (blockSideSzPx * (x - halfFieldWidthBlk) + halfBlkPx))
                                        (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                        $ displayBlock ((fld !! y) !! x)
                                        $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                                        ++ (displayField_auc fld (x + 1) y)

-- handle input
handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey key) Down _ _) state | (key == KeyRight) = checkCorrectMove state 1 0 0
                                                       | (key == KeyLeft) = checkCorrectMove state (-1) 0 0
                                                       | (key == KeyDown) = checkCorrectMove state 0 1 0
                                                       | (key == KeyUp)   = dropFigure state
handleInput (EventKey (Char key) Down _ _) state | (key == 'r') = checkCorrectMove state 0 0 90
handleInput _ state = state -- ignore other events

-- simulation step
update :: Float -> State -> State
update _ state@(State {..}) = checkCorrectMove state 0 1 0



main :: IO ()
main = do
    gen <- getStdGen

    let 
        randFigIdxList = randomRs range gen
        initFig = listOfFigures !! (head randFigIdxList)
        initRandFigIdxList = tail randFigIdxList
        initState = State emptyField initFig halfFieldWidthBlk 0 0 initRandFigIdxList initialScore initialSpeed initTickCnt

    play 
        displayMode -- open a new window
        backgroundColor -- background color
        stepsPerSecond -- number of simulation steps per second
        initState -- initial state
        displayField -- display function
        handleInput -- input handler
        update