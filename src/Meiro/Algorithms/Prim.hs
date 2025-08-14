module Meiro.Algorithms.Prim
    ( -- * Prim's Algorithm
      generatePrimMaze
    , PrimWall(..)
    ) where

import System.Random (StdGen, randomR)
import Data.Set (Set)
import qualified Data.Set as Set
import Meiro.Types

-- | Represents a wall in Prim's algorithm
data PrimWall = PrimWall Position Position deriving (Eq, Ord, Show)

-- | Prim's minimum spanning tree maze generation algorithm
generatePrimMaze :: [[Cell]] -> Int -> Int -> Position -> StdGen -> ([[Cell]], StdGen)
generatePrimMaze _ width height startPos gen = 
    let -- Initialize grid with all walls
        initialGrid = replicate height (replicate width Wall)
        -- Mark starting cell as path
        gridWithStart = setCellAt startPos Path initialGrid
        -- Initialize visited set with starting position
        visited = Set.singleton startPos
        -- Get initial walls from starting position
        initialWalls = getNeighborWalls startPos width height
        -- Process using Prim's algorithm
        (finalGrid, finalGen) = primAlgorithm gridWithStart initialWalls visited gen width height
    in (finalGrid, finalGen)

-- | Main Prim's algorithm loop
primAlgorithm :: [[Cell]] -> [PrimWall] -> Set Position -> StdGen -> Int -> Int -> ([[Cell]], StdGen)
primAlgorithm grid [] _ gen _ _ = (grid, gen)
primAlgorithm grid walls visited gen width height =
    if null walls
    then (grid, gen)
    else
        let -- Pick a random wall
            (randomIndex, newGen) = randomR (0, length walls - 1) gen
            PrimWall cell1 cell2 = walls !! randomIndex
            remainingWalls = take randomIndex walls ++ drop (randomIndex + 1) walls
        in if Set.member cell1 visited && not (Set.member cell2 visited)
           then -- cell1 is visited, cell2 is not - connect them
               let wallPos = getWallBetween cell1 cell2
                   newGrid = setCellAt cell2 Path (setCellAt wallPos Path grid)
                   newVisited = Set.insert cell2 visited
                   newWalls = remainingWalls ++ getNeighborWalls cell2 width height
               in primAlgorithm newGrid newWalls newVisited newGen width height
           else if Set.member cell2 visited && not (Set.member cell1 visited)
           then -- cell2 is visited, cell1 is not - connect them
               let wallPos = getWallBetween cell1 cell2
                   newGrid = setCellAt cell1 Path (setCellAt wallPos Path grid)
                   newVisited = Set.insert cell1 visited
                   newWalls = remainingWalls ++ getNeighborWalls cell1 width height
               in primAlgorithm newGrid newWalls newVisited newGen width height
           else -- Both visited or both unvisited - skip this wall
               primAlgorithm grid remainingWalls visited newGen width height

-- | Get walls to neighboring unvisited cells
getNeighborWalls :: Position -> Int -> Int -> [PrimWall]
getNeighborWalls (x, y) width height = 
    let candidates = [(x, y-2), (x, y+2), (x-2, y), (x+2, y)]  -- 2 steps away
        validCandidates = filter (\(nx, ny) -> 
            nx > 0 && nx < width-1 && ny > 0 && ny < height-1) candidates
        walls = [PrimWall (x, y) neighbor | neighbor <- validCandidates]
    in walls

-- | Get the wall position between two cells
getWallBetween :: Position -> Position -> Position
getWallBetween (x1, y1) (x2, y2) = 
    ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

-- | Set cell at specific position
setCellAt :: Position -> Cell -> [[Cell]] -> [[Cell]]
setCellAt (x, y) newCell grid =
    case splitAt y grid of
        (before, row:after) -> 
            let newRow = take x row ++ [newCell] ++ drop (x + 1) row
            in before ++ [newRow] ++ after
        _ -> grid  -- Return unchanged grid if index out of bounds