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
displayField state@(State { appState = StartScreen }) = Pictures [Scale 0.6 0.6 $ color white $ Text "TetriHS",
              Translate 0 (fromIntegral $ (-2) * blockSideSzPx) $ Scale 0.3 0.3 $ color white $ Text "Press Enter to start"]
displayField state@(State {..}) = Pictures $ (displayField_auc (addFigToField fld (figureToField fig) x y alpha) 0 0) ++ 
                                              makeGrid ++ [Translate 30 0 $ Scale 0.5 0.5 $ color white $ Text $ "Score: " ++ (show score)]

displayField_auc :: Field -> Xcoord -> Ycoord -> [Picture]
displayField_auc fld (Xcoord x) (Ycoord y) | (x == (fieldWidthBlk - 1)) && 
                            (y == (fieldHeightBlk - 1)) 
                                    = [(Translate (fromIntegral (blockSideSzPx * halfFieldWidthBlk - halfBlkPx) - (fromIntegral halfInfoWidthPx))
                                       (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                       $ displayBlock ((fld !! y) !! x)
                                       $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                         | (x >= fieldWidthBlk)
                                    = displayField_auc fld (Xcoord 0) (Ycoord (y + 1))
                         | otherwise
                                    =  [(Translate (fromIntegral $ (blockSideSzPx * (x - halfFieldWidthBlk) + halfBlkPx) - (fromIntegral halfInfoWidthPx))
                                        (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                        $ displayBlock ((fld !! y) !! x)
                                        $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                                        ++ (displayField_auc fld (Xcoord (x + 1)) (Ycoord y))

-- handle input
handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey key) Down _ _) state@(State { appState = StartScreen }) | (key == KeyEnter) = state { appState = InGame }
handleInput (EventKey (SpecialKey key) Down _ _) state | (key == KeyRight) = checkCorrectMove state 1 0 0
                                                       | (key == KeyLeft) = checkCorrectMove state (-1) 0 0
                                                       | (key == KeyDown) = checkCorrectMove state 0 1 0
                                                       | (key == KeyUp)   = dropFigure state
handleInput (EventKey (Char key) Down _ _) state | (key == 'r') = checkCorrectMove state 0 0 90
handleInput _ state = state -- ignore other events

-- simulation step
update :: Float -> State -> State
update _ state@(State { appState = StartScreen }) = state
update _ state@(State {..}) = checkCorrectMove state 0 1 0



main :: IO ()
main = do
    gen <- getStdGen

    let 
        randFigIdxList = randomRs range gen
        initFig = listOfFigures !! (head randFigIdxList)
        initRandFigIdxList = tail randFigIdxList
        initState = State emptyField initFig (Xcoord halfFieldWidthBlk) (Ycoord 0) (RotateDegree 0) initRandFigIdxList initialScore initialSpeed initTickCnt StartScreen

    play 
        displayMode -- open a new window
        backgroundColor -- background color
        stepsPerSecond -- number of simulation steps per second
        initState -- initial state
        displayField -- display function
        handleInput -- input handler
        update