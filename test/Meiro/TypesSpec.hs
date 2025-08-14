module Meiro.TypesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Time (getCurrentTime)
import Meiro.Types

spec :: Spec
spec = describe "Meiro.Types" $ do
  
  describe "Position validation" $ do
    it "validates positions within bounds" $ do
      isValidPosition (5, 5) (10, 10) `shouldBe` True
      isValidPosition (0, 0) (10, 10) `shouldBe` True
      isValidPosition (9, 9) (10, 10) `shouldBe` True
    
    it "rejects positions outside bounds" $ do
      isValidPosition (-1, 0) (10, 10) `shouldBe` False
      isValidPosition (0, -1) (10, 10) `shouldBe` False
      isValidPosition (10, 5) (10, 10) `shouldBe` False
      isValidPosition (5, 10) (10, 10) `shouldBe` False

  describe "Maze size validation" $ do
    it "accepts valid maze sizes" $ do
      isValidMazeSize (3, 3) `shouldBe` True
      isValidMazeSize (5, 7) `shouldBe` True
      isValidMazeSize (11, 13) `shouldBe` True
    
    it "rejects invalid maze sizes" $ do
      isValidMazeSize (2, 2) `shouldBe` False    -- too small
      isValidMazeSize (4, 4) `shouldBe` False    -- even numbers
      isValidMazeSize (3, 4) `shouldBe` False    -- one even
      isValidMazeSize (0, 0) `shouldBe` False    -- zero

  describe "Smart constructors" $ do
    it "creates valid maze with mkMaze" $ do
      let grid = [[Wall, Wall, Wall], [Wall, Path, Wall], [Wall, Wall, Wall]]
          result = mkMaze 3 3 grid (1, 1) (1, 1)
      result `shouldSatisfy` isRight
    
    it "rejects invalid maze parameters" $ do
      let grid = [[Wall, Wall, Wall], [Wall, Path, Wall], [Wall, Wall, Wall]]
      mkMaze 2 3 grid (1, 1) (1, 1) `shouldSatisfy` isLeft
      mkMaze 3 3 [] (1, 1) (1, 1) `shouldSatisfy` isLeft
      mkMaze 3 3 grid (-1, 1) (1, 1) `shouldSatisfy` isLeft

  describe "Direction enumeration" $ do
    it "has all cardinal directions" $ do
      [North, South, East, West] `shouldBe` [minBound..maxBound]

-- Helper for Either results
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight