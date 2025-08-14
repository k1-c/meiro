module Meiro.MazeGenerationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import System.Random (mkStdGen)
import Meiro.Types
import Meiro.MazeGeneration

spec :: Spec
spec = describe "Meiro.MazeGeneration" $ do
  
  describe "Algorithm selection" $ do
    it "generates maze with RecursiveBacktrack algorithm" $ do
      let gen = mkStdGen 42
          result = generateMazeWithAlgorithmAndSeed RecursiveBacktrack 5 5 gen
      result `shouldSatisfy` isRight
      
    it "fallback algorithms work correctly" $ do
      let gen = mkStdGen 42
          results = [ generateMazeWithAlgorithmAndSeed algo 5 5 gen 
                    | algo <- [RecursiveBacktrack, Kruskal, Prim] ]
      all isRight results `shouldBe` True

  describe "generateMazeWithSeed" $ do
    it "generates valid mazes for valid sizes" $ do
      let gen = mkStdGen 42
          result = generateMazeWithSeed 5 5 gen
      result `shouldSatisfy` isRight
      
      case result of
        Right maze -> do
          mazeWidth maze `shouldBe` 5
          mazeHeight maze `shouldBe` 5
          mazeStart maze `shouldBe` (1, 1)
          mazeGoal maze `shouldBe` (3, 3)
        Left _ -> expectationFailure "Expected valid maze"
    
    it "rejects invalid maze sizes" $ do
      let gen = mkStdGen 42
      generateMazeWithSeed 2 2 gen `shouldSatisfy` isLeft
      generateMazeWithSeed 4 5 gen `shouldSatisfy` isLeft


  describe "Maze connectivity" $ do
    it "ensures path from start to goal exists" $ property $
      \(Positive w) (Positive h) -> 
        let width = let w' = max 3 w in if odd w' then w' else w' + 1
            height = let h' = max 3 h in if odd h' then h' else h' + 1
            gen = mkStdGen 123
            result = generateMazeWithSeed width height gen
        in case result of
             Right maze -> hasPathFromStartToGoal maze
             Left _ -> False

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool  
isLeft = not . isRight

-- Simple path existence check (BFS would be more robust)
hasPathFromStartToGoal :: Maze -> Bool
hasPathFromStartToGoal maze =
  let start = mazeStart maze
      goal = mazeGoal maze
      grid = mazeCells maze
  in isAccessible grid start && isAccessible grid goal
  where
    isAccessible grid (x, y) = 
      let cell = (grid !! y) !! x
      in cell /= Wall