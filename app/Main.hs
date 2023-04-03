{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Color
import System.Random

-- TODO: change to "newtype"
-- newtype Xcoord = Xcoord Int deriving (Num, Show)
-- newtype Ycoord = Ycoord Int deriving (Num, Show)
-- newtype RotateDegree = RotateDegree Int deriving (Num, Show)

type Xcoord = Int
type Ycoord = Int
type RotateDegree = Int

-- "Overlay" value is useful when checking if figure movement is possible
data Block = Empty | Edge | Cyan | Blue | Orange | Yellow | Green | Purple | Red | Overlay deriving (Show, Eq)
type FieldLine = [Block]
type Field = [FieldLine]

data Figure = I | J | L | O | S | T | Z deriving (Show)

data State = State { fld :: Field
                   , fig :: Figure
                   , x :: Xcoord
                   , y :: Ycoord
                   , alpha :: RotateDegree
                   , randFigIdxList :: [Int]
                   , score :: Int
                   , speed :: Int
                   , tickCount :: Int
                   } 

and' :: Block -> Block -> Block
and' Empty Empty = Empty
and' Empty blk = blk
and' blk Empty = blk
and' _ _ = Overlay

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

transpose :: Field -> Field
transpose ([]:_) = []
transpose fig = (map head fig) : (transpose $ map tail fig)

rotateClockwiseOnce :: Field -> Field
rotateClockwiseOnce fig = transpose $ reverse fig

rotateClockwise :: Field -> RotateDegree -> Field
rotateClockwise fig 0 = fig
rotateClockwise fig 360 = fig
rotateClockwise fig 90 = rotateClockwiseOnce fig
rotateClockwise fig 180 = rotateClockwiseOnce $ 
                          rotateClockwiseOnce fig
rotateClockwise fig 270 = rotateClockwiseOnce $ 
                          rotateClockwiseOnce $
                          rotateClockwiseOnce fig

listOfFigures :: [Figure]
listOfFigures = [I, J, L, O, S, T, Z]
                         
addFigToField :: Field -> Field -> Xcoord -> Ycoord -> RotateDegree -> Field
addFigToField fld fig x y alpha = map (\(a, b) -> (map (\(blk1, blk2) -> blk1 `and'` blk2)) (zip a b)) (zip fld ext_fig)
        where rotatedFig = rotateClockwise fig alpha;
              ext_fig = (replicate y (replicate fieldWidthBlk Empty)) ++
                        (map (\ln -> (replicate x Empty) ++ 
                                     ln ++ 
                                     (replicate (fieldWidthBlk - length ln - x) Empty)) rotatedFig) ++ 
                        (replicate (fieldHeightBlk - length rotatedFig - y) (replicate fieldWidthBlk Empty))

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

backgroundColor :: Color
backgroundColor = makeColorI 30 30 30 0

emptyLine :: FieldLine
emptyLine = [Edge] ++ (replicate (fieldWidthBlk - 2) Empty) ++ [Edge]

emptyField :: Field
emptyField = [replicate fieldWidthBlk Empty] ++ 
             (replicate (fieldHeightBlk - 2) emptyLine) ++ 
             [(replicate fieldWidthBlk Edge)]

isFull :: FieldLine -> Bool
isFull ln = (length $ takeWhile (\blk -> blk /= Empty) ln) == length ln

updateField :: Field -> Field
updateField fld = [(replicate fieldWidthBlk Empty)] ++ replicate (length fld - length left - 1) emptyLine ++ (drop 1 left) ++ [(replicate fieldWidthBlk Edge)]
        where left = filter (\ln -> not $ isFull ln) fld

setLine :: Field -> Ycoord -> FieldLine -> Field
setLine fld idx ln = (take idx fld) ++ [ln] ++ (take (fieldHeightBlk - 1 - idx) fld)

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

range :: (Int, Int)
range = (0, length listOfFigures - 1)

stepsPerSecond :: Int
stepsPerSecond = 1

f :: Field
f = emptyField

-- Game display mode.
displayMode :: Display
displayMode = InWindow "Tetris" (fieldWidthPx, fieldHeightPx) (50, 50) 

main :: IO ()
main = do
    gen <- getStdGen
    let 
        randFigIdxList = randomRs range gen
        initFig = listOfFigures !! (head randFigIdxList)
        initRandFigIdxList = tail randFigIdxList
        initState = State emptyField initFig halfFieldWidthBlk 0 0 initRandFigIdxList initialScore initialSpeed initTickCnt

    play displayMode -- open a new window
        backgroundColor -- background color
        stepsPerSecond -- number of simulation steps per second
        initState -- initial state
        displayField -- display function
        handleInput -- input handler
        update

displayField :: State -> Picture
displayField state@(State {..}) = Pictures $ makeGrid ++ (displayField_auc (addFigToField fld (figureToField fig) x y alpha) 0 0)

-- simulation step
update :: Float -> State -> State
update _ state@(State {..}) = checkCorrectMove state 0 1 0

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

makeGrid :: [Picture]
makeGrid = map (\path -> (Line path)) ((map (\x -> [(x, 11 * fromIntegral(blockSideSzPx)), (x, (-11) * fromIntegral(blockSideSzPx))]) 
                [x * fromIntegral(blockSideSzPx) | x <- [(-6),(-5)..6]]) ++
           (map (\y -> [(6 * fromIntegral(blockSideSzPx), y), ((-6) * fromIntegral(blockSideSzPx), y)]) 
                [y * fromIntegral(blockSideSzPx) | y <- [(-11),(-10)..11]]))

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
              maxDiff = (length $ takeWhile (/= Overlay) (map (\yDiff -> checkCorrectMove_auc (addFigToField fld figField x (y + yDiff) alpha) 0 0) [0,1..(fieldHeightBlk - y)])) - 1;
              newFld = addFigToField fld (figureToField fig) x (y + maxDiff) alpha

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
              -- rotatedFigure = rotateClockwise figField alpha;
              figWidth = length (figField !! 0);
              figHeight = length figField;
              newFld = addFigToField fld figField (x + xDiff) (y + yDiff) (alpha + alphaDiff);
              isCorrect = checkCorrectMove_auc newFld 0 0;


checkCorrectMove_auc :: Field -> Xcoord -> Ycoord -> Block
checkCorrectMove_auc fld x y | (x == (fieldWidthBlk - 1)) && (y == (fieldHeightBlk - 1)) = ((fld !! y) !! x)
                             | (x >= fieldWidthBlk)                                      = checkCorrectMove_auc fld 0 (y + 1)
                             | ((fld !! y) !! x) == Overlay                              = Overlay
                             | otherwise                                                 = checkCorrectMove_auc fld (x + 1) y

handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey key) Down _ _) state | (key == KeyRight) = checkCorrectMove state 1 0 0
                                                       | (key == KeyLeft) = checkCorrectMove state (-1) 0 0
                                                       | (key == KeyDown) = checkCorrectMove state 0 1 0
                                                       | (key == KeyUp)   = dropFigure state
handleInput (EventKey (Char key) Down _ _) state | (key == 'r') = checkCorrectMove state 0 0 90
handleInput _ state = state -- ignore other events