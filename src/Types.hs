{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Xcoord (..)
    , Ycoord (..)
    , RotateDegree (..)
    , xToInt
    , yToInt
    , degToInt
    , Block (..)
    , Field
    , FieldLine
    , Figure (..)
    , State (..)
    ) where

----------------------------------------
{- 
DATA 
TYPES
-}
----------------------------------------

newtype Xcoord = Xcoord Int deriving (Num, Eq, Ord)
newtype Ycoord = Ycoord Int deriving (Num, Eq, Ord)
newtype RotateDegree = RotateDegree Int deriving (Num, Eq, Ord, Real, Enum, Integral)

xToInt :: Xcoord -> Int
xToInt (Xcoord x) = x

yToInt :: Ycoord -> Int
yToInt (Ycoord y) = y

degToInt :: RotateDegree -> Int
degToInt (RotateDegree alpha) = alpha

instance Show Xcoord where
  show (Xcoord x) = show x

instance Show Ycoord where
  show (Ycoord y) = show y

instance Show RotateDegree where
  show (RotateDegree alpha) = show alpha

-- "Overlay" value is useful when checking if figure movement is possible
-- "Edge" value is used for buliding field boundaries
data Block = Empty | Edge | Cyan | Blue | Orange | Yellow | Green | Purple | Red | Overlay deriving (Show, Eq)
type FieldLine = [Block]
type Field = [FieldLine]

data Figure = I | J | L | O | S | T | Z deriving (Show)

data State = State { fld :: Field -- consists of blocks that are NOT moving anywhere
                   , fig :: Figure -- type of descending tetramino
                   , x :: Xcoord -- x coordinate of figure
                   , y :: Ycoord -- y coordinate of figure
                   , alpha :: RotateDegree -- angle by which tetramino is rotated
                   , randFigIdxList :: [Int] -- infinite list of random indexes, 
                                 -- used to get new falling figures
                   , score :: Int -- current score
                   , speed :: Int -- current fall speed 
                   , tickCount :: Int -- counter of ticks passed since the start
                   } deriving (Show)