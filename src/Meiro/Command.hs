module Meiro.Command
    ( runCommand
    , Config(..)
    , Action(..)
    , parseArgs
    , parseSize
    , parseAlgorithm
    , defaultConfig
    ) where

import Meiro.Types
import Meiro.MazeGeneration (generateMazeWithAlgorithm, MazeAlgorithm(..))
import Meiro.Gameplay (playGame, GameResult(..))
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

-- | Configuration for the CLI application
data Config = Config
    { configWidth :: Int
    , configHeight :: Int
    , configAlgorithm :: MazeAlgorithm
    , configAction :: Action
    } deriving (Show, Eq)

-- | Actions that the CLI can perform
data Action = PlayGame | ShowHelp | ShowVersion | InvalidArgs String
    deriving (Show, Eq)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
    { configWidth = 25
    , configHeight = 15
    , configAlgorithm = RecursiveBacktrack
    , configAction = PlayGame
    }

-- | Main command entry point
runCommand :: [String] -> IO ()
runCommand args = do
    let config = parseArgs args defaultConfig
    case configAction config of
        ShowHelp -> showHelp
        ShowVersion -> putStrLn "meiro version 0.1.0.0"
        PlayGame -> runGame config
        InvalidArgs err -> do
            putStrLn err
            showHelp
            exitWith (ExitFailure 1)

-- | Parse command-line arguments into a configuration
parseArgs :: [String] -> Config -> Config
parseArgs [] config = config
parseArgs ("--help":_) config = config { configAction = ShowHelp }
parseArgs ("-h":_) config = config { configAction = ShowHelp }
parseArgs ("--version":_) config = config { configAction = ShowVersion }
parseArgs ("-v":_) config = config { configAction = ShowVersion }
parseArgs ("--size":sizeStr:rest) config = 
    case parseSize sizeStr of
        Just (w, h) -> parseArgs rest (config { configWidth = w, configHeight = h })
        Nothing -> config { configAction = InvalidArgs $ "Invalid size: " ++ sizeStr ++ ". Both width and height must be odd numbers >= 5 (e.g., 25x15, 31x21)" }
parseArgs ("-s":sizeStr:rest) config = parseArgs ("--size":sizeStr:rest) config
parseArgs ("--algorithm":algoStr:rest) config = 
    case parseAlgorithm algoStr of
        Just algo -> parseArgs rest (config { configAlgorithm = algo })
        Nothing -> config { configAction = InvalidArgs $ "Unknown algorithm: " ++ algoStr ++ ". Use: recursive, kruskal, or prim" }
parseArgs ("-a":algoStr:rest) config = parseArgs ("--algorithm":algoStr:rest) config
parseArgs (unknown:_) config = config { configAction = InvalidArgs $ "Unknown option: " ++ unknown }

-- | Parse size string in "WxH" format
parseSize :: String -> Maybe (Int, Int)
parseSize str = 
    case break (== 'x') str of
        (widthStr, 'x':heightStr) -> do
            width <- readMaybe widthStr
            height <- readMaybe heightStr
            -- Both dimensions must be odd and >= 5
            if width >= 5 && odd width && height >= 5 && odd height
                then Just (width, height)
                else Nothing
        _ -> Nothing

-- | Parse algorithm string
parseAlgorithm :: String -> Maybe MazeAlgorithm
parseAlgorithm "recursive" = Just RecursiveBacktrack
parseAlgorithm "kruskal" = Just Kruskal
parseAlgorithm "prim" = Just Prim
parseAlgorithm _ = Nothing

-- | Show help message
showHelp :: IO ()
showHelp = do
    putStrLn "Meiro - Terminal-based maze generation & solving game"
    putStrLn ""
    putStrLn "Usage: meiro [options]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --size WxH, -s WxH      Maze dimensions (width x height)"
    putStrLn "                          Default: 25x15"
    putStrLn "                          Both width and height must be odd numbers >= 5"
    putStrLn ""
    putStrLn "  --algorithm, -a         Generation algorithm"
    putStrLn "                          Options: recursive, kruskal, prim"
    putStrLn "                          Default: recursive"
    putStrLn ""
    putStrLn "  --help, -h              Show this help message"
    putStrLn "  --version, -v           Show version information"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  meiro                          # Start with default settings"
    putStrLn "  meiro --size 41x31             # 41x31 maze"
    putStrLn "  meiro -s 51x51 -a kruskal      # 51x51 maze with Kruskal's algorithm"
    putStrLn ""
    putStrLn "Controls:"
    putStrLn "  WASD or Arrow Keys - Move around"
    putStrLn "  Q - Quit game"

-- | Run the game with the given configuration
runGame :: Config -> IO ()
runGame config = do
    result <- generateMazeWithAlgorithm (configAlgorithm config) (configWidth config) (configHeight config)
    case result of
        Right maze -> do
            putStrLn $ "Starting game with " ++ algorithmName (configAlgorithm config) ++ " algorithm..."
            putStrLn $ "Maze size: " ++ show (configWidth config) ++ "x" ++ show (configHeight config)
            gameResult <- playGame maze
            case gameResult of
                Victory moves time -> do
                    putStrLn $ "🎉 Congratulations! You reached the goal!"
                    putStrLn $ "Final stats - Moves: " ++ show moves ++ ", Time: " ++ show time ++ " seconds"
                Quit -> putStrLn "Game ended by user."
                Error err -> do
                    putStrLn $ "Game error: " ++ err
                    exitWith (ExitFailure 1)
        Left err -> do
            putStrLn $ "Error generating maze: " ++ err
            exitWith (ExitFailure 1)

-- | Get algorithm display name
algorithmName :: MazeAlgorithm -> String
algorithmName RecursiveBacktrack = "Recursive Backtracking"
algorithmName Kruskal = "Kruskal's Algorithm"
algorithmName Prim = "Prim's Algorithm"