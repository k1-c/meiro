module Meiro.Algorithms.Kruskal
    ( -- * Kruskal's Algorithm
      generateKruskalMaze
    , KruskalWall(..)
    , UnionFind
    ) where

import System.Random (StdGen)
import Data.Array (Array, array, (!), (//))
import Meiro.Types
import Meiro.Utils

-- | Represents a wall between two cells
data KruskalWall = KruskalWall Position Position deriving (Eq, Show)

-- | Union-Find data structure for tracking connected components
type UnionFind = Array Int Int

-- | Kruskal's minimum spanning tree maze generation algorithm
generateKruskalMaze :: [[Cell]] -> Int -> Int -> Position -> StdGen -> ([[Cell]], StdGen)
generateKruskalMaze _ width height _ gen = 
    let -- Initialize grid with all walls
        initialGrid = replicate height (replicate width Meiro.Types.Wall)
        -- Get all possible walls between adjacent cells
        allWalls = getAllWalls width height
        -- Shuffle the walls randomly
        (shuffledWalls, newGen) = fisherYatesShuffle allWalls gen
        -- Initialize Union-Find structure
        cellCount = ((width-1) `div` 2 + 1) * ((height-1) `div` 2 + 1)
        unionFind = array (0, cellCount-1) [(i, i) | i <- [0..cellCount-1]]
        -- Process walls using Kruskal's algorithm
        finalGrid = processWallsKruskal initialGrid shuffledWalls unionFind width height
    in (finalGrid, newGen)

-- | Get all possible walls between adjacent cells in the grid
getAllWalls :: Int -> Int -> [KruskalWall]
getAllWalls width height = 
    let -- Only consider odd positions as valid cells (1,1), (1,3), (3,1), etc.
        cells = [(x, y) | x <- [1, 3..width-2], y <- [1, 3..height-2]]
        -- Create walls between adjacent cells
        horizontalWalls = [KruskalWall (x, y) (x+2, y) | (x, y) <- cells, x+2 < width-1]
        verticalWalls = [KruskalWall (x, y) (x, y+2) | (x, y) <- cells, y+2 < height-1]
    in horizontalWalls ++ verticalWalls

-- | Process walls using Kruskal's algorithm with Union-Find
processWallsKruskal :: [[Cell]] -> [KruskalWall] -> UnionFind -> Int -> Int -> [[Cell]]
processWallsKruskal grid [] _ _ _ = grid
processWallsKruskal grid (wall:walls) uf width height =
    let KruskalWall (x1, y1) (x2, y2) = wall
        cell1Id = positionToId (x1, y1) width
        cell2Id = positionToId (x2, y2) width
        root1 = findRoot uf cell1Id
        root2 = findRoot uf cell2Id
    in if root1 /= root2
       then -- Cells are in different components, connect them
           let newUF = union uf root1 root2
               -- Remove the wall between cells
               wallPos = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
               gridWithCells = setCellAt (x1, y1) Path (setCellAt (x2, y2) Path grid)
               gridWithWall = setCellAt wallPos Path gridWithCells
           in processWallsKruskal gridWithWall walls newUF width height
       else -- Cells are already connected, skip this wall
           processWallsKruskal grid walls uf width height

-- | Convert position to unique cell ID for Union-Find
positionToId :: Position -> Int -> Int
positionToId (x, y) width = 
    let cellX = x `div` 2
        cellY = y `div` 2
        cellsPerRow = (width - 1) `div` 2 + 1
    in cellY * cellsPerRow + cellX

-- | Find the root of a set in Union-Find structure
findRoot :: UnionFind -> Int -> Int
findRoot uf x = 
    let parent = uf ! x
    in if parent == x
       then x
       else findRoot uf parent

-- | Union two sets in Union-Find structure
union :: UnionFind -> Int -> Int -> UnionFind
union uf root1 root2 = uf // [(root2, root1)]

-- | Set cell at specific position
setCellAt :: Position -> Cell -> [[Cell]] -> [[Cell]]
setCellAt (x, y) newCell grid =
    case splitAt y grid of
        (before, row:after) ->
            let newRow = take x row ++ [newCell] ++ drop (x + 1) row
            in before ++ [newRow] ++ after
        _ -> grid  -- Return unchanged grid if index out of bounds