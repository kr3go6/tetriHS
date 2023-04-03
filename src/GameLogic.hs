{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameLogic
    ( and'
    , displayBlock
    , isFull
    , addFigToField
    , updateField
    , figureToField
    , transpose
    , rotateClockwise
    , dropFigure
    , checkCorrectMove
    ) where

import Types
import ConstValues
import Graphics.Gloss

----------------------------------------
{- 
FUNCTIONS
-}
----------------------------------------



-- logical and for blocks
-- used in checking if figure position is acceptable
and' :: Block -> Block -> Block
and' Empty Empty = Empty
and' Empty blk = blk
and' blk Empty = blk
and' _ _ = Overlay

displayBlock :: Block -> (Picture -> Picture)
displayBlock Edge = color $ greyN 0.5
displayBlock Empty = color backgroundColor
displayBlock Cyan = color cyan
displayBlock Blue = color blue
displayBlock Orange = color orange
displayBlock Yellow = color yellow
displayBlock Green = color green
displayBlock Purple = color violet
displayBlock Red = color red
displayBlock Overlay = color white

-- check if line should be cleared
isFull :: FieldLine -> Bool
isFull ln = (length $ takeWhile (\blk -> blk /= Empty) ln) == length ln

-- return field with figure placed in it
addFigToField :: Field -> Field -> Xcoord -> Ycoord -> RotateDegree -> Field
addFigToField fld fig x y alpha = map (\(a, b) -> (map (\(blk1, blk2) -> blk1 `and'` blk2)) (zip a b)) (zip fld ext_fig)
        where rotatedFig = rotateClockwise fig alpha;
              ext_fig = (replicate y (replicate fieldWidthBlk Empty)) ++
                        (map (\ln -> (replicate x Empty) ++ 
                                     ln ++ 
                                     (replicate (fieldWidthBlk - length ln - x) Empty)) rotatedFig) ++ 
                        (replicate (fieldHeightBlk - length rotatedFig - y) (replicate fieldWidthBlk Empty))

-- clear all lines that are full
updateField :: Field -> Field
updateField fld = [(replicate fieldWidthBlk Empty)] ++ 
            replicate (length fld - length left - 1) emptyLine ++ 
            (drop 1 left) ++ [(replicate fieldWidthBlk Edge)]
        where left = filter (\ln -> not $ isFull ln) fld

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

-- transpose matrix
transpose :: Field -> Field
transpose ([]:_) = []
transpose fig = (map head fig) : (transpose $ map tail fig)

-- rotate figure once
rotateClockwiseOnce :: Field -> Field
rotateClockwiseOnce fig = transpose $ reverse fig

-- rotate by angle
rotateClockwise :: Field -> RotateDegree -> Field
rotateClockwise fig 0 = fig
rotateClockwise fig 360 = fig
rotateClockwise fig 90 = rotateClockwiseOnce fig
rotateClockwise fig 180 = rotateClockwiseOnce $ 
                          rotateClockwiseOnce fig
rotateClockwise fig 270 = rotateClockwiseOnce $ 
                          rotateClockwiseOnce $
                          rotateClockwiseOnce fig

-- place tetramino to lowest possible position
-- return state with updated field and new figure
-- (called when KeyUp is pressed)
dropFigure :: State -> State
dropFigure state@(State {..}) = state { fld = updateField newFld 
                                      , fig = listOfFigures !! (head randFigIdxList `mod` 7)
                                      , x = 4
                                      , y = 0
                                      , alpha = 0
                                      , randFigIdxList = tail randFigIdxList
                                      } 
        where figField = figureToField fig;
              rotatedFigure = rotateClockwise figField alpha;
              maxDiff = (length $ takeWhile (/= Overlay) 
                    (map (\yDiff -> checkCorrectMove_auc (addFigToField fld figField x (y + yDiff)
                    alpha) 0 0) [0,1..(fieldHeightBlk - y)])) - 1;
              newFld = addFigToField fld (figureToField fig) x (y + maxDiff) alpha

checkCorrectMove_auc :: Field -> Xcoord -> Ycoord -> Block
checkCorrectMove_auc fld x y | (x == (fieldWidthBlk - 1)) && (y == (fieldHeightBlk - 1)) = ((fld !! y) !! x)
                             | (x >= fieldWidthBlk)                                      = checkCorrectMove_auc fld 0 (y + 1)
                             | ((fld !! y) !! x) == Overlay                              = Overlay
                             | otherwise                                                 = checkCorrectMove_auc fld (x + 1) y

-- check if tetramino position is possible (no overlays on field)
-- if not possible, return previous state
-- if possible, return updated state
checkCorrectMove :: State -> Xcoord -> Ycoord -> RotateDegree -> State
checkCorrectMove state@(State {..}) xDiff yDiff alphaDiff | (x + xDiff + figWidth > fieldWidthBlk) || 
                                                (x + xDiff < 0) ||
                                                (y + yDiff < 0)     
                                                    = state
                                                | (isCorrect == Overlay && yDiff > 0)
                                                    = state { fld = updateField $ 
                                                                    addFigToField fld figField x y alpha
                                                            , fig = listOfFigures !! (head randFigIdxList `mod` 7)
                                                            , x = halfFieldWidthBlk
                                                            , y = 0
                                                            , alpha = 0
                                                            , randFigIdxList = tail randFigIdxList
                                                            } 
                                                | (isCorrect /= Overlay)    = state { x = x + xDiff
                                                                                    , y = y + yDiff
                                                                                    , alpha = (alpha + alphaDiff) `mod` 360
                                                                                    }
                                                | otherwise                 = state
        where figField = figureToField fig;
              figWidth = length (figField !! 0);
              figHeight = length figField;
              newFld = addFigToField fld figField (x + xDiff) (y + yDiff) (alpha + alphaDiff);
              isCorrect = checkCorrectMove_auc newFld 0 0;
