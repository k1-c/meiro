-- | Main library module that re-exports the public API
module Lib
    ( -- * Re-exports from Meiro modules
      -- ** Types
      module Meiro.Types
      -- ** Maze Generation
    , generateMaze
    , generateMazeWithSeed
    , generateMazeWithAlgorithm
    , generateMazeWithAlgorithmAndSeed
    , MazeAlgorithm(..)
      -- ** Gameplay
    , playGame
    , GameResult(..)
      -- ** Rendering
    , renderMaze
    , renderMazeToString
    , CellStyle(..)
    , defaultCellStyle
    ) where

-- Re-exports
import Meiro.Types
import Meiro.MazeGeneration (generateMaze, generateMazeWithSeed, generateMazeWithAlgorithm, generateMazeWithAlgorithmAndSeed, MazeAlgorithm(..))
import Meiro.Gameplay (playGame, GameResult(..))
import Meiro.Rendering (renderMaze, renderMazeToString, CellStyle(..), defaultCellStyle)