module Meiro.Rendering
    ( -- * Maze Rendering
      renderMaze
    , renderMazeToString
    -- * Cell Rendering
    , renderCell
    , renderRow
    , CellStyle(..)
    , defaultCellStyle
    -- * Maze Manipulation
    , updateMazeWithPlayer
    -- * Screen Management
    , clearScreenAndRender
    , displayGameInfo
    ) where

import System.Console.ANSI
import Meiro.Types

-- | Cell rendering style configuration
data CellStyle = CellStyle
    { wallChar   :: String
    , pathChar   :: String
    , startChar  :: String
    , goalChar   :: String
    , playerChar :: String
    } deriving (Eq, Show)

-- | Default Unicode/Emoji cell style
defaultCellStyle :: CellStyle
defaultCellStyle = CellStyle
    { wallChar   = "██"
    , pathChar   = "  "
    , startChar  = "🏠"
    , goalChar   = "🏁"
    , playerChar = "🤖"
    }

-- | Render maze to the console with player position
renderMaze :: Maze -> Position -> IO ()
renderMaze maze playerPos = do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ renderMazeToString maze playerPos defaultCellStyle

-- | Clear screen and render maze (for game loop)
clearScreenAndRender :: Maze -> Position -> IO ()
clearScreenAndRender = renderMaze

-- | Convert maze to string representation
renderMazeToString :: Maze -> Position -> CellStyle -> String
renderMazeToString maze playerPos style = 
    let grid = mazeCells maze
        gridWithPlayer = updateMazeWithPlayer grid playerPos
    in unlines $ map (renderRow style) gridWithPlayer

-- | Update maze grid to show player position
updateMazeWithPlayer :: [[Cell]] -> Position -> [[Cell]]
updateMazeWithPlayer grid (px, py) =
    let (before, row:after) = splitAt py grid
        newRow = take px row ++ [Player] ++ drop (px + 1) row
    in before ++ [newRow] ++ after

-- | Render a single row of the maze
renderRow :: CellStyle -> [Cell] -> String
renderRow style = concatMap (renderCell style)

-- | Render a single cell using the specified style
renderCell :: CellStyle -> Cell -> String
renderCell style cell = case cell of
    Wall   -> wallChar style
    Path   -> pathChar style
    Start  -> startChar style
    Goal   -> goalChar style
    Player -> playerChar style

-- | Display game information (moves, position, etc.)
displayGameInfo :: GameState -> IO ()
displayGameInfo gameState = do
    let moves = gsMoves gameState
    
    putStrLn ""
    putStrLn $ "Moves: " ++ show moves
    putStrLn "Controls: WASD or Arrow keys to move, 'q' to quit"