module Main (main) where

import Lib
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> handlePlay "15" RecursiveBacktrack  -- Default: play mode with size 15
        ["--version"] -> putStrLn "meiro version 0.1.0.0"
        ["--generate", sizeStr] -> handleGenerate sizeStr RecursiveBacktrack
        ["--generate", sizeStr, "--algorithm", algoStr] -> do
            case parseAlgorithm algoStr of
                Just algo -> handleGenerate sizeStr algo
                Nothing -> do
                    putStrLn $ "Unknown algorithm: " ++ algoStr
                    showAlgorithms
                    exitWith (ExitFailure 1)
        ["--play"] -> handlePlay "15" RecursiveBacktrack  -- Default size 15
        ["--play", sizeStr] -> handlePlay sizeStr RecursiveBacktrack
        ["--play", sizeStr, "--algorithm", algoStr] -> do
            case parseAlgorithm algoStr of
                Just algo -> handlePlay sizeStr algo
                Nothing -> do
                    putStrLn $ "Unknown algorithm: " ++ algoStr
                    showAlgorithms
                    exitWith (ExitFailure 1)
        ["--algorithms"] -> showAlgorithms
        _ -> do
            putStrLn "Invalid arguments."
            showHelp
            exitWith (ExitFailure 1)

showHelp :: IO ()
showHelp = do
    putStrLn "Meiro - Professional Maze Generator and Game"
    putStrLn "Usage: meiro [options]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  (no options)                        Play a maze with default size 15"
    putStrLn "  --generate <size>                   Generate and display a maze"
    putStrLn "  --generate <size> --algorithm <algo> Generate maze with specific algorithm"
    putStrLn "  --play                              Play a maze with default size 15"
    putStrLn "  --play <size>                       Generate and play a maze interactively"
    putStrLn "  --play <size> --algorithm <algo>    Play maze with specific algorithm"
    putStrLn "  --algorithms                        List available algorithms"
    putStrLn "  --version                           Show version information"
    putStrLn ""
    putStrLn "Size must be an odd number >= 5 (e.g., 5, 7, 9, 11, 15, 25)"
    putStrLn "Default size is 15 when not specified."
    putStrLn ""
    putStrLn "Run --algorithms to see available maze generation algorithms"

showAlgorithms :: IO ()
showAlgorithms = do
    putStrLn "Available maze generation algorithms:"
    putStrLn ""
    putStrLn "  recursive-backtrack  Classic recursive backtracking (default)"
    putStrLn "  kruskal              Kruskal's minimum spanning tree algorithm"  
    putStrLn "  prim                 Prim's minimum spanning tree algorithm"
    putStrLn ""
    putStrLn "All algorithms are fully implemented and generate different maze characteristics."

handleGenerate :: String -> MazeAlgorithm -> IO ()
handleGenerate sizeStr algorithm = 
    case parseMazeSize sizeStr of
        Just size -> do
            result <- generateMazeWithAlgorithm algorithm size size
            case result of
                Right maze -> do
                    renderMaze maze (mazeStart maze)
                    putStrLn $ "\nMaze generated! Size: " ++ show size ++ "x" ++ show size
                    putStrLn $ "Algorithm: " ++ algorithmName algorithm
                    putStrLn $ "Start: " ++ show (mazeStart maze)
                    putStrLn $ "Goal: " ++ show (mazeGoal maze)
                Left err -> do
                    putStrLn $ "Error generating maze: " ++ err
                    exitWith (ExitFailure 1)
        Nothing -> do
            putStrLn "Invalid size. Size must be an odd number >= 5"
            exitWith (ExitFailure 1)

handlePlay :: String -> MazeAlgorithm -> IO ()
handlePlay sizeStr algorithm =
    case parseMazeSize sizeStr of
        Just size -> do
            result <- generateMazeWithAlgorithm algorithm size size
            case result of
                Right maze -> do
                    putStrLn $ "Starting game with " ++ algorithmName algorithm ++ " algorithm..."
                    gameResult <- playGame maze
                    case gameResult of
                        Victory moves time -> do
                            putStrLn $ "Game completed successfully!"
                            putStrLn $ "Final stats - Moves: " ++ show moves ++ ", Time: " ++ show time
                        Quit -> putStrLn "Game ended by user."
                        Error err -> do
                            putStrLn $ "Game error: " ++ err
                            exitWith (ExitFailure 1)
                Left err -> do
                    putStrLn $ "Error generating maze: " ++ err
                    exitWith (ExitFailure 1)
        Nothing -> do
            putStrLn "Invalid size. Size must be an odd number >= 5"
            exitWith (ExitFailure 1)

parseMazeSize :: String -> Maybe Int
parseMazeSize sizeStr = do
    size <- readMaybe sizeStr
    if size >= 5 && odd size
        then Just size
        else Nothing

parseAlgorithm :: String -> Maybe MazeAlgorithm
parseAlgorithm "recursive-backtrack" = Just RecursiveBacktrack
parseAlgorithm "kruskal" = Just Kruskal
parseAlgorithm "prim" = Just Prim
parseAlgorithm _ = Nothing

algorithmName :: MazeAlgorithm -> String
algorithmName RecursiveBacktrack = "Recursive Backtracking"
algorithmName Kruskal = "Kruskal's Algorithm"
algorithmName Prim = "Prim's Algorithm"
