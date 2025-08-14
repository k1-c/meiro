module Meiro.MazeGeneration
    ( -- * Maze Generation
      generateMaze
    , generateMazeWithSeed
    , generateMazeWithAlgorithm
    , generateMazeWithAlgorithmAndSeed
    -- * Algorithm Types
    , MazeAlgorithm(..)
    ) where

import System.Random (StdGen, newStdGen)
import Meiro.Types

-- Algorithm imports
import qualified Meiro.Algorithms.RecursiveBacktrack as RB
import qualified Meiro.Algorithms.Kruskal as K
import qualified Meiro.Algorithms.Prim as P

-- | Available maze generation algorithms
data MazeAlgorithm 
    = RecursiveBacktrack -- ^ Classic recursive backtracking (default)
    | Kruskal           -- ^ Kruskal's algorithm (future)  
    | Prim              -- ^ Prim's algorithm (future)
    deriving (Eq, Show, Enum, Bounded)

-- | Generate a maze with a random seed (using default algorithm)
generateMaze :: Int -> Int -> IO (Either String Maze)
generateMaze width height = do
    gen <- newStdGen
    return $ generateMazeWithSeed width height gen

-- | Generate a maze with specified algorithm and random seed
generateMazeWithAlgorithm :: MazeAlgorithm -> Int -> Int -> IO (Either String Maze)
generateMazeWithAlgorithm algorithm width height = do
    gen <- newStdGen
    return $ generateMazeWithAlgorithmAndSeed algorithm width height gen

-- | Generate a maze with a specific seed for reproducibility (using default algorithm)
generateMazeWithSeed :: Int -> Int -> StdGen -> Either String Maze
generateMazeWithSeed = generateMazeWithAlgorithmAndSeed RecursiveBacktrack

-- | Generate a maze with specified algorithm and seed for reproducibility
generateMazeWithAlgorithmAndSeed :: MazeAlgorithm -> Int -> Int -> StdGen -> Either String Maze
generateMazeWithAlgorithmAndSeed algorithm width height gen
    | not (isValidMazeSize (width, height)) = Left "Invalid maze size"
    | otherwise = 
        let startPos = (1, 1)
            goalPos = (width - 2, height - 2)
            initialGrid = replicate height (replicate width Wall)
            (finalGrid, _) = runMazeAlgorithm algorithm initialGrid width height startPos gen
            mazeResult = do
                maze <- mkMaze width height finalGrid startPos goalPos
                maze' <- setCell maze startPos Start
                setCell maze' goalPos Goal
        in mazeResult

-- | Dispatch to the appropriate maze generation algorithm
runMazeAlgorithm :: MazeAlgorithm -> [[Cell]] -> Int -> Int -> Position -> StdGen -> ([[Cell]], StdGen)
runMazeAlgorithm RecursiveBacktrack = RB.generateRecursiveBacktrackMaze
runMazeAlgorithm Kruskal = K.generateKruskalMaze
runMazeAlgorithm Prim = P.generatePrimMaze

-- | Set a cell in the maze (for smart constructor)
setCell :: Maze -> Position -> Cell -> Either String Maze
setCell maze pos newCell
    | not (isValidPosition pos (mazeWidth maze, mazeHeight maze)) = 
        Left "Invalid position for setCell"
    | otherwise = 
        let newCells = updateCellAt newCell pos (mazeCells maze)
        in Right maze { mazeCells = newCells }

-- | Update cell at specific position
updateCellAt :: Cell -> Position -> [[Cell]] -> [[Cell]]
updateCellAt newCell (x, y) grid =
    let (before, row:after) = splitAt y grid
        newRow = take x row ++ [newCell] ++ drop (x + 1) row
    in before ++ [newRow] ++ after