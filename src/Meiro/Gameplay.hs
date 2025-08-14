module Meiro.Gameplay
    ( -- * Game State Management
      initializeGame
    , playGame
    -- * Player Movement
    , movePlayer
    , tryMove
    , isValidMove
    , moveInDirection
    -- * Game Loop
    , gameLoop
    , GameResult(..)
    -- * Input Handling
    , getInput
    , handleKeyInput
    , parseDirection
    ) where

import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(..))
import System.Console.ANSI (hideCursor, showCursor)
import Control.Exception (finally)

import Meiro.Types
import Meiro.Rendering
import Meiro.Utils

-- | Possible game results
data GameResult = Victory Int NominalDiffTime | Quit | Error String
    deriving (Eq, Show)

-- | Initialize a new game with the given maze
initializeGame :: Maze -> IO GameState
initializeGame maze = do
    startTime <- getCurrentTime
    return $ mkGameState (mazeStart maze) 0 startTime maze

-- | Main game entry point
playGame :: Maze -> IO GameResult
playGame maze = do
    gameState <- initializeGame maze
    
    -- Setup terminal
    setupTerminal
    
    -- Display welcome message
    displayWelcome
    
    -- Start game loop
    result <- gameLoop gameState `finally` restoreTerminal
    
    return result

-- | Setup terminal for game input
setupTerminal :: IO ()
setupTerminal = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor

-- | Restore terminal settings
restoreTerminal :: IO ()
restoreTerminal = do
    showCursor
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering

-- | Display welcome message
displayWelcome :: IO ()
displayWelcome = do
    putStrLn "🎮 Meiro - Maze Game"
    putStrLn "Use WASD or arrow keys to move, 'q' to quit"
    putStrLn "Press any key to start..."
    _ <- safeGetChar
    return ()

-- | Main game loop
gameLoop :: GameState -> IO GameResult
gameLoop gameState = do
    let playerPos = gsPlayerPos gameState
        maze = gsMaze gameState
    
    -- Render current state
    clearScreenAndRender maze playerPos
    displayGameInfo gameState
    
    -- Check victory condition
    if playerPos == mazeGoal maze
    then do
        endTime <- getCurrentTime
        let totalTime = diffUTCTime endTime (gsStartTime gameState)
            moves = gsMoves gameState
        displayVictory moves totalTime
        return $ Victory moves totalTime
    else do
        -- Get input and continue
        input <- getInput
        case input of
            Just QuitGame -> return Quit
            Just (Move dir) -> do
                let newGameState = tryMove dir gameState
                gameLoop newGameState
            Nothing -> gameLoop gameState  -- Invalid input, continue

-- | Handle keyboard input
data InputAction = Move Direction | QuitGame
    deriving (Eq, Show)

-- | Get input including arrow keys
getInput :: IO (Maybe InputAction)
getInput = do
    key <- safeGetChar
    case key of
        '\ESC' -> do
            -- Check if it's an arrow key sequence
            key2 <- safeGetChar
            if key2 == '['
            then do
                key3 <- safeGetChar
                case key3 of
                    'A' -> return $ Just (Move North)  -- Up arrow
                    'B' -> return $ Just (Move South)  -- Down arrow
                    'C' -> return $ Just (Move East)   -- Right arrow
                    'D' -> return $ Just (Move West)   -- Left arrow
                    _   -> return Nothing
            else return Nothing
        _ -> return $ handleKeyInput key

handleKeyInput :: Char -> Maybe InputAction
handleKeyInput 'q' = Just QuitGame
handleKeyInput 'w' = Just (Move North)
handleKeyInput 'a' = Just (Move West)
handleKeyInput 's' = Just (Move South)
handleKeyInput 'd' = Just (Move East)
handleKeyInput _ = Nothing

-- | Parse direction from character
parseDirection :: Char -> Maybe Direction
parseDirection 'w' = Just North
parseDirection 'a' = Just West
parseDirection 's' = Just South
parseDirection 'd' = Just East
parseDirection _ = Nothing

-- | Try to move player in given direction
tryMove :: Direction -> GameState -> GameState
tryMove dir gameState =
    let currentPos = gsPlayerPos gameState
        newPos = moveInDirection currentPos dir
        maze = gsMaze gameState
    in if isValidMove maze newPos
       then gameState { gsPlayerPos = newPos, gsMoves = gsMoves gameState + 1 }
       else gameState

-- | Move position in given direction
moveInDirection :: Position -> Direction -> Position
moveInDirection (x, y) North = (x, y - 1)
moveInDirection (x, y) South = (x, y + 1)
moveInDirection (x, y) East  = (x + 1, y)
moveInDirection (x, y) West  = (x - 1, y)

-- | Check if move to position is valid
isValidMove :: Maze -> Position -> Bool
isValidMove maze pos@(x, y) =
    let grid = mazeCells maze
        width = mazeWidth maze
        height = mazeHeight maze
    in x >= 0 && x < width && y >= 0 && y < height &&
       (grid !! y !! x) /= Wall

-- | Move player (pure function)
movePlayer :: Direction -> GameState -> GameState
movePlayer = tryMove

-- | Display victory message
displayVictory :: Int -> NominalDiffTime -> IO ()
displayVictory moves time = do
    putStrLn "\n🎉 Congratulations! You reached the goal!"
    putStrLn $ "Total moves: " ++ show moves
    putStrLn $ "Time: " ++ formatGameTime time
    putStrLn "Press any key to exit..."
    _ <- safeGetChar
    return ()