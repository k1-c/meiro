module Meiro.GameplaySpec (spec) where

import Test.Hspec
import System.Random (mkStdGen)
import Meiro.Types
import Meiro.MazeGeneration
import Meiro.Gameplay

spec :: Spec
spec = describe "Meiro.Gameplay" $ do
  
  describe "Movement" $ do
    let testMaze = case generateMazeWithSeed 5 5 (mkStdGen 42) of
                     Right maze -> maze
                     Left _ -> error "Failed to generate test maze"
    
    it "moves player in valid directions" $ do
      let currentTime = read "2025-01-01 00:00:00 UTC"
          gameState = mkGameState (1, 1) 0 currentTime testMaze
          -- Try all directions and find one that works
          eastState = tryMove East gameState
          southState = tryMove South gameState
          westState = tryMove West gameState
          northState = tryMove North gameState
          -- At least one direction should be valid (move count increases)
          allStates = [eastState, southState, westState, northState]
          validMoves = filter (\s -> gsMoves s > 0) allStates
      
      -- At least one move should be possible from start position
      length validMoves `shouldSatisfy` (> 0)

    it "prevents movement into walls" $ do
      let currentTime = read "2025-01-01 00:00:00 UTC"
          gameState = mkGameState (1, 1) 0 currentTime testMaze
          -- Try to move into a wall (assuming (0,1) is a wall)
          newState = tryMove West gameState
      
      -- Position shouldn't change if move is invalid
      gsPlayerPos newState `shouldBe` gsPlayerPos gameState

    it "increments move count on valid moves" $ do
      let currentTime = read "2025-01-01 00:00:00 UTC"
          gameState = mkGameState (1, 1) 5 currentTime testMaze
          newState = tryMove South gameState
      
      if gsPlayerPos newState /= gsPlayerPos gameState
        then gsMoves newState `shouldBe` 6
        else gsMoves newState `shouldBe` 5

  describe "Direction parsing" $ do
    it "parses WASD keys correctly" $ do
      parseDirection 'w' `shouldBe` Just North
      parseDirection 'a' `shouldBe` Just West
      parseDirection 's' `shouldBe` Just South
      parseDirection 'd' `shouldBe` Just East
    
    it "rejects invalid characters" $ do
      parseDirection 'x' `shouldBe` Nothing
      parseDirection '1' `shouldBe` Nothing

  describe "Movement validation" $ do
    let grid = [[Wall, Wall, Wall],
                [Wall, Path, Wall],
                [Wall, Wall, Wall]]
        testMaze = case mkMaze 3 3 grid (1, 1) (1, 1) of
                     Right maze -> maze
                     Left _ -> error "Failed to create test maze"
    
    it "validates moves within maze bounds" $ do
      isValidMove testMaze (1, 1) `shouldBe` True   -- Path cell
      isValidMove testMaze (0, 0) `shouldBe` False  -- Wall cell
      isValidMove testMaze (-1, 0) `shouldBe` False -- Out of bounds
      isValidMove testMaze (3, 1) `shouldBe` False  -- Out of bounds

  describe "Position movement" $ do
    it "moves positions correctly in all directions" $ do
      moveInDirection (5, 5) North `shouldBe` (5, 4)
      moveInDirection (5, 5) South `shouldBe` (5, 6)
      moveInDirection (5, 5) East  `shouldBe` (6, 5)
      moveInDirection (5, 5) West  `shouldBe` (4, 5)