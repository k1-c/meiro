{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Meiro.Types
    ( -- * Basic Types
      Position
    , Direction(..)
    , Cell(..)
    -- * Maze Structure
    , Maze(..)
    , MazeSize
    -- * Game State
    , GameState(..)
    -- * Smart Constructors
    , mkMaze
    , mkGameState
    -- * Validation Functions
    , isValidPosition
    , isValidMazeSize
    ) where

import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Position on the maze grid (x, y)
type Position = (Int, Int)

-- | Size of the maze (width, height)
type MazeSize = (Int, Int)

-- | Cardinal directions for movement
data Direction = North | South | East | West
    deriving (Eq, Show, Enum, Bounded, Generic)

-- | Cell types in the maze
data Cell = Wall | Path | Start | Goal | Player
    deriving (Eq, Show, Generic)

-- | Maze data structure containing the grid and metadata
data Maze = Maze
    { mazeWidth  :: !Int
    , mazeHeight :: !Int
    , mazeCells  :: ![[Cell]]
    , mazeStart  :: !Position
    , mazeGoal   :: !Position
    } deriving (Eq, Show, Generic)

-- | Game state containing current position, moves, and timing
data GameState = GameState
    { gsPlayerPos :: !Position
    , gsMoves     :: !Int
    , gsStartTime :: !UTCTime
    , gsMaze      :: !Maze
    } deriving (Show, Generic)

-- | Smart constructor for creating a valid maze
mkMaze :: Int -> Int -> [[Cell]] -> Position -> Position -> Either String Maze
mkMaze width height cells start goal
    | not (isValidMazeSize (width, height)) = Left "Invalid maze size: must be odd and >= 3"
    | not (isValidPosition start (width, height)) = Left "Invalid start position"
    | not (isValidPosition goal (width, height)) = Left "Invalid goal position"
    | length cells /= height = Left "Grid height mismatch"
    | any ((/= width) . length) cells = Left "Grid width mismatch"
    | otherwise = Right $ Maze width height cells start goal

-- | Smart constructor for creating a game state
mkGameState :: Position -> Int -> UTCTime -> Maze -> GameState
mkGameState = GameState

-- | Check if a position is valid within the maze bounds
isValidPosition :: Position -> MazeSize -> Bool
isValidPosition (x, y) (width, height) = 
    x >= 0 && x < width && y >= 0 && y < height

-- | Check if maze size is valid (must be odd and >= 3)
isValidMazeSize :: MazeSize -> Bool
isValidMazeSize (width, height) = 
    width >= 3 && height >= 3 && odd width && odd height