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
addFigToField fld fig (Xcoord x) (Ycoord y) (RotateDegree alpha) = map (\(a, b) -> (map (\(blk1, blk2) -> blk1 `and'` blk2)) (zip a b)) (zip fld ext_fig)
        where rotatedFig = rotateClockwise fig (RotateDegree alpha);
              ext_fig
                    | y >= 0    = (replicate y (replicate fieldWidthBlk Empty)) ++
                                (map (\ln -> (replicate x Empty) ++ 
                                             ln ++ 
                                             (replicate (fieldWidthBlk - length ln - x) Empty)) rotatedFig) ++ 
                                (replicate (fieldHeightBlk - length rotatedFig - y) (replicate fieldWidthBlk Empty))
                    | y < 0     = (map (\ln -> (replicate x Empty) ++ 
                                             ln ++ 
                                             (replicate (fieldWidthBlk - length ln - x) Empty)) (reverse $ take (length rotatedFig + y) $ reverse rotatedFig)) ++ 
                                (replicate (fieldHeightBlk - length rotatedFig - y) (replicate fieldWidthBlk Empty))

-- clear all lines that are full
clearLines :: Field -> Field
clearLines fld = filter (\ln -> not $ isFull ln) fld

-- restore field after cleaning
updateField :: Field -> Field
updateField fld = [(replicate fieldWidthBlk Empty)] ++ 
            replicate (fieldHeightBlk - length fld - 1) emptyLine ++ 
            (drop 1 fld) ++ [(replicate fieldWidthBlk Edge)]

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
rotateClockwise fig (RotateDegree 0) = fig
rotateClockwise fig (RotateDegree 360) = fig
rotateClockwise fig (RotateDegree 90) = rotateClockwiseOnce fig
rotateClockwise fig (RotateDegree 180) = rotateClockwiseOnce $ 
                                         rotateClockwiseOnce fig
rotateClockwise fig (RotateDegree 270) = rotateClockwiseOnce $ 
                                         rotateClockwiseOnce $
                                         rotateClockwiseOnce fig

-- place tetramino to lowest possible position
-- return state with updated field and new figure
-- (called when KeyUp is pressed)
dropFigure :: State -> State
dropFigure state@(State {..}) = checkCorrectMove state { fld = updateField cleanedNewFld 
                                      , fig = listOfFigures !! (head randFigIdxList `mod` 7)
                                      , x = (Xcoord halfFieldWidthBlk)
                                      , y = (Ycoord 0)
                                      , alpha = (RotateDegree 0)
                                      , randFigIdxList = tail randFigIdxList
                                      , score = score + earnedScores (fieldHeightBlk - length cleanedNewFld - 1)
                                      } 0 0 0
        where figField = figureToField fig;
              rotatedFigure = rotateClockwise figField alpha;
              maxDiff = (length $ takeWhile (/= Overlay) 
                    (map (\yDiff -> checkCorrectMove_auc (addFigToField fld figField x (y + (Ycoord yDiff))
                    alpha) 0 0) [0,1..(fieldHeightBlk - yToInt y)])) - 1;
              newFld = addFigToField fld (figureToField fig) x (y + (Ycoord maxDiff)) alpha;
              cleanedNewFld = clearLines newFld;

checkCorrectMove_auc :: Field -> Xcoord -> Ycoord -> Block
checkCorrectMove_auc fld (Xcoord x) (Ycoord y) | (x == (fieldWidthBlk - 1)) && (y == (fieldHeightBlk - 1)) = ((fld !! y) !! x)
                             | (x >= fieldWidthBlk)                                      = checkCorrectMove_auc fld (Xcoord 0) (Ycoord (y + 1))
                             | ((fld !! y) !! x) == Overlay                              = Overlay
                             | otherwise                                                 = checkCorrectMove_auc fld (Xcoord (x + 1)) (Ycoord y)

-- check if tetramino position is possible (no overlays on field)
-- if not possible, return previous state
-- if possible, return updated state
checkCorrectMove :: State -> Xcoord -> Ycoord -> RotateDegree -> State
checkCorrectMove state@(State {..}) xDiff yDiff alphaDiff | (x + xDiff + (Xcoord figWidth) > (Xcoord fieldWidthBlk)) || 
                                                (x + xDiff < 0)
                                                    = state
                                                | isCorrect == Overlay && y + yDiff < 0
                                                    = checkCorrectMove state { appState = Finished
                                                                             , y = y - 1
                                                                             } 0 0 0
                                                | y + yDiff < 0
                                                    = state { appState = Finished }
                                                | (isCorrect == Overlay && yDiff > 0)
                                                    = checkCorrectMove state { fld = updateField cleanedNewFld
                                                            , fig = listOfFigures !! (head randFigIdxList `mod` 7)
                                                            , x = (Xcoord halfFieldWidthBlk)
                                                            , y = 0
                                                            , alpha = 0
                                                            , randFigIdxList = tail randFigIdxList
                                                            , score = score + earnedScores (fieldHeightBlk - length cleanedNewFld - 1)
                                                            } 0 0 0
                                                | (isCorrect /= Overlay)    = state { x = x + xDiff
                                                                                    , y = y + yDiff
                                                                                    , alpha = (alpha + alphaDiff) `mod` 360
                                                                                    }
                                                | (isCorrect == Overlay && y + yDiff == 0)
                                                    = checkCorrectMove state { appState = Finished 
                                                                             , y = y - 1
                                                                             } 0 0 0
                                                | otherwise                 = state
        where figField = figureToField fig;
              figWidth = length (figField !! 0);
              figHeight = length figField;
              newFld = addFigToField fld figField (x + xDiff) (y + yDiff) (alpha + alphaDiff);
              isCorrect = checkCorrectMove_auc newFld 0 0;
              cleanedNewFld = clearLines $ addFigToField fld figField x y alpha;
