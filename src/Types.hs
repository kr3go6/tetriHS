{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Xcoord
    , Ycoord
    , RotateDegree
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

type Xcoord = Int
type Ycoord = Int
type RotateDegree = Int

-- newtype Xcoord = Xcoord Int deriving (Num)
-- newtype Ycoord = Ycoord Int deriving (Num)
-- newtype RotateDegree = RotateDegree Int deriving (Num)

-- instance Show Xcoord where
  -- show (Xcoord x) = show x

-- instance Show Ycoord where
  -- show (Ycoord y) = show y

-- instance Show RotateDegree where
  -- show (RotateDegree alpha) = show alpha

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