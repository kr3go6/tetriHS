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

-- display one figure out of field
displayFigure :: Field -> Xcoord -> Ycoord -> [Picture]
displayFigure fld (Xcoord x) (Ycoord y) | (x == (fwb - 1)) && 
                            (y == (fhb - 1)) 
                                    = [(Translate (fromIntegral (round ((fromIntegral blockSideSzPx) * half_fwb - (fromIntegral halfBlkPx))))
                                       (fromIntegral (round (fromIntegral blockSideSzPx * (half_fhb - fromIntegral y) - fromIntegral halfBlkPx)))
                                       $ displayBlock ((fld !! y) !! x)
                                       $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                         | (x >= fwb)
                                    = displayFigure fld (Xcoord 0) (Ycoord (y + 1))
                         | otherwise
                                    =  [(Translate (fromIntegral (round (fromIntegral blockSideSzPx * (fromIntegral x - half_fwb) + fromIntegral halfBlkPx)))
                                        (fromIntegral (round (fromIntegral blockSideSzPx * (half_fhb - fromIntegral y) - fromIntegral halfBlkPx)))
                                        $ displayBlock ((fld !! y) !! x)
                                        $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                                        ++ (displayFigure fld (Xcoord (x + 1)) (Ycoord y))
        where   fwb = length (fld !! 0);
                fhb = length fld;
                half_fwb = ((fromIntegral fwb) :: Double) / 2
                half_fhb = ((fromIntegral fhb) :: Double) / 2

-- display info about next three figures
nextFiguresInfo :: State -> [Picture]
nextFiguresInfo state@(State {..}) = [Scale 0.4 0.4 $ Translate 450 950 $ color white $ Text "Next:",
                Scale 0.6 0.6 $ Translate 200 500 $ Pictures $ displayFigure (figureToField $ listOfFigures !! (head randFigIdxList `mod` 7)) 0 0,
                Scale 0.6 0.6 $ Translate 400 500 $ Pictures $ displayFigure (figureToField $ listOfFigures !! (head (tail randFigIdxList) `mod` 7)) 0 0,
                Scale 0.6 0.6 $ Translate 600 500 $ Pictures $ displayFigure (figureToField $ listOfFigures !! (head (tail $ tail randFigIdxList) `mod` 7)) 0 0]

-- display all objects
displayField :: State -> Picture
displayField state@(State { appState = StartScreen }) = Pictures [Scale 0.6 0.6 $ color white $ Text "TetriHS",
              Translate 0 (fromIntegral $ (-2) * blockSideSzPx) $ Scale 0.3 0.3 $ color white $ Text "Press Enter to start"]
-- displayField state@(State { appState = Finished, ..}) = Pictures [Scale 0.6 0.6 $ color white $ Text "Game Over",
--               Translate 0 (fromIntegral $ (-2) * blockSideSzPx) $ Scale 0.4 0.4 $ color white $ Text ("You scored: " ++ (show score))]
displayField state@(State { appState = Finished, .. }) = Pictures $ (displayField_auc (addFigToField fld (figureToField fig) x y alpha) 0 0) ++ 
                                              makeGrid ++ [Translate 30 0 $ Scale 0.5 0.5 $ color white $ Text $ "Score: " ++ (show score),
                                                           Translate 30 (-80) $ Scale 0.5 0.5 $ color white $ Text "GAME OVER"]
displayField state@(State {..}) = Pictures $ (displayGhost (ghostTetramino state) 0 0) ++ (nextFiguresInfo state) ++ (displayField_auc (addFigToField fld (figureToField fig) x y alpha) 0 0) ++ 
                                              makeGrid ++ [Translate 30 0 $ Scale 0.5 0.5 $ color white $ Text $ "Score: " ++ (show score)]

displayGhost :: Field -> Xcoord -> Ycoord -> [Picture]
displayGhost fld (Xcoord x) (Ycoord y) | (x == (fieldWidthBlk - 1)) && 
                            (y == (fieldHeightBlk - 1)) 
                                    = [(Translate (fromIntegral (blockSideSzPx * halfFieldWidthBlk - halfBlkPx) - (fromIntegral halfInfoWidthPx))
                                       (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                       $ (displayGhostBlock 0.4 ((fld !! y) !! x))
                                       $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                         | (x >= fieldWidthBlk)
                                    = displayGhost fld (Xcoord 0) (Ycoord (y + 1))
                         | otherwise
                                    =  [(Translate (fromIntegral $ (blockSideSzPx * (x - halfFieldWidthBlk) + halfBlkPx) - (fromIntegral halfInfoWidthPx))
                                        (fromIntegral (blockSideSzPx * (halfFieldHeightBlk - y) - halfBlkPx))
                                        $ (displayGhostBlock 0.4 ((fld !! y) !! x))
                                        $ rectangleSolid (fromIntegral blockSideSzPx) (fromIntegral blockSideSzPx))]
                                        ++ (displayGhost fld (Xcoord (x + 1)) (Ycoord y))

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
handleInput (EventKey (SpecialKey key) Down _ _) state@(State { appState = Finished }) = state
handleInput (EventKey (SpecialKey key) Down _ _) state@(State { appState = InGame }) | (key == KeyRight) = checkCorrectMove state 1 0 0
                                                                                     | (key == KeyLeft) = checkCorrectMove state (-1) 0 0
                                                                                     | (key == KeyDown) = checkCorrectMove state 0 1 0
                                                                                     | (key == KeyUp)   = dropFigure state
handleInput (EventKey (Char key) Down _ _) state | (key == 'r') = checkCorrectMove state 0 0 90
handleInput _ state = state -- ignore other events

-- simulation step
update :: Float -> State -> State
update _ state@(State { appState = StartScreen }) = state
update _ state@(State { appState = Finished }) = state
update _ state@(State {..}) | waitTicks == 1   = checkCorrectMove state { tickCount = tickCount + 1
                                                                        , waitTicks = waitTicks - 1
                                                                        } 0 0 0
                            | waitTicks > 0    = state { tickCount = tickCount + 1
                                                       , waitTicks = waitTicks - 1
                                                       }
                            | (tickCount `mod` speed) == 0       = checkCorrectMove state { tickCount = tickCount + 1 } 0 1 0
                            | otherwise                          = state { tickCount = tickCount + 1 }


main :: IO ()
main = do
    gen <- getStdGen

    let 
        randFigIdxList = randomRs range gen
        initFig = listOfFigures !! (head randFigIdxList)
        initRandFigIdxList = tail randFigIdxList
        initState = State emptyField initFig (Xcoord halfFieldWidthBlk) (Ycoord 0) (RotateDegree 0) initRandFigIdxList initialScore initialSpeed initTickCnt StartScreen 0

    play 
        displayMode -- open a new window
        backgroundColor -- background color
        stepsPerSecond -- number of simulation steps per second
        initState -- initial state
        displayField -- display function
        handleInput -- input handler
        update