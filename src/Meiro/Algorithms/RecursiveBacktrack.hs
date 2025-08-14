module Meiro.Algorithms.RecursiveBacktrack
    ( -- * Recursive Backtracking Algorithm
      generateRecursiveBacktrackMaze
    ) where

import System.Random (StdGen, randomR)
import Meiro.Types

-- | Recursive backtracking maze generation algorithm
generateRecursiveBacktrackMaze :: [[Cell]] -> Int -> Int -> Position -> StdGen -> ([[Cell]], StdGen)
generateRecursiveBacktrackMaze _ width height startPos gen = 
    let -- Initialize grid with all walls
        initialGrid = replicate height (replicate width Wall)
        -- Start the recursive backtracking from start position
        (finalGrid, finalGen) = recursiveBacktrack initialGrid [startPos] startPos gen width height
    in (finalGrid, finalGen)

-- | Main recursive backtracking algorithm
recursiveBacktrack :: [[Cell]] -> [Position] -> Position -> StdGen -> Int -> Int -> ([[Cell]], StdGen)
recursiveBacktrack grid [] _ gen _ _ = (grid, gen)
recursiveBacktrack grid stack@(current:rest) _ gen width height =
    let -- Mark current cell as part of the maze (Path)
        gridWithCurrent = setCellAt current Path grid
        -- Get unvisited neighbors
        unvisitedNeighbors = getUnvisitedNeighbors gridWithCurrent current width height
    in if null unvisitedNeighbors
       then -- Backtrack: no unvisited neighbors
           let nextPos = if null rest then current else safeHead rest current
               safeHead [] def = def
               safeHead (h:_) _ = h
           in recursiveBacktrack gridWithCurrent rest nextPos gen width height
       else -- Choose a random neighbor
           let (randomIndex, newGen) = randomR (0, length unvisitedNeighbors - 1) gen
               chosenNeighbor = unvisitedNeighbors !! randomIndex
               -- Remove wall between current and chosen neighbor
               wallPos = getWallBetween current chosenNeighbor
               gridWithPath = setCellAt wallPos Path gridWithCurrent
               -- Add chosen neighbor to stack and continue
               newStack = chosenNeighbor : stack
           in recursiveBacktrack gridWithPath newStack chosenNeighbor newGen width height

-- | Get unvisited neighbors (cells that are 2 steps away and still walls)
getUnvisitedNeighbors :: [[Cell]] -> Position -> Int -> Int -> [Position]
getUnvisitedNeighbors grid (x, y) width height =
    let candidates = [(x, y-2), (x, y+2), (x-2, y), (x+2, y)]  -- 2 steps away
        validCandidates = filter (\(nx, ny) -> 
            nx > 0 && nx < width-1 && ny > 0 && ny < height-1) candidates
        unvisited = filter (\pos -> getCellAt grid pos == Wall) validCandidates
    in unvisited

-- | Get the wall position between two cells
getWallBetween :: Position -> Position -> Position
getWallBetween (x1, y1) (x2, y2) = 
    ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

-- | Get cell at specific position
getCellAt :: [[Cell]] -> Position -> Cell
getCellAt grid (x, y) = (grid !! y) !! x

-- | Set cell at specific position
setCellAt :: Position -> Cell -> [[Cell]] -> [[Cell]]
setCellAt (x, y) newCell grid =
    case splitAt y grid of
        (before, row:after) ->
            let newRow = take x row ++ [newCell] ++ drop (x + 1) row
            in before ++ [newRow] ++ after
        _ -> grid  -- Return unchanged grid if index out of bounds