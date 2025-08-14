module Meiro.RenderingSpec (spec) where

import Test.Hspec
import Meiro.Types
import Meiro.Rendering

spec :: Spec
spec = describe "Meiro.Rendering" $ do
  
  describe "Cell rendering" $ do
    let style = defaultCellStyle
    
    it "renders cells with correct characters" $ do
      renderCell style Wall `shouldBe` "██"
      renderCell style Path `shouldBe` "  "
      renderCell style Start `shouldBe` "🏠"
      renderCell style Goal `shouldBe` "🏁"
      renderCell style Player `shouldBe` "🤖"

  describe "Row rendering" $ do
    let style = defaultCellStyle
        row = [Wall, Path, Player, Goal, Wall]
        expected = "██  🤖🏁██"
    
    it "renders complete rows correctly" $ do
      renderRow style row `shouldBe` expected

  describe "Maze string rendering" $ do
    let grid = [[Wall, Wall, Wall],
                [Wall, Path, Wall], 
                [Wall, Wall, Wall]]
        maze = case mkMaze 3 3 grid (1, 1) (1, 1) of
                 Right m -> m
                 Left _ -> error "Failed to create test maze"
        playerPos = (1, 1)
        style = defaultCellStyle
        
    it "renders complete maze with player" $ do
      let result = renderMazeToString maze playerPos style
          lines' = lines result
      length lines' `shouldBe` 3
      -- First and last rows should be all walls
      head lines' `shouldBe` "██████"
      last lines' `shouldBe` "██████"
      -- Middle row should have player
      (lines' !! 1) `shouldContain` "🤖"

  describe "Custom cell styles" $ do
    let customStyle = CellStyle "##" ".." "SS" "GG" "PP"
        
    it "uses custom characters for rendering" $ do
      renderCell customStyle Wall `shouldBe` "##"
      renderCell customStyle Path `shouldBe` ".."
      renderCell customStyle Start `shouldBe` "SS"
      renderCell customStyle Goal `shouldBe` "GG"
      renderCell customStyle Player `shouldBe` "PP"

  describe "Player position updating" $ do
    let grid = [[Wall, Wall, Wall],
                [Wall, Path, Wall],
                [Wall, Wall, Wall]]
        playerPos = (1, 1)
        updatedGrid = updateMazeWithPlayer grid playerPos
        
    it "places player at specified position" $ do
      let playerCell = (updatedGrid !! 1) !! 1
      playerCell `shouldBe` Player
      
    it "preserves other cells" $ do
      let topLeft = (updatedGrid !! 0) !! 0
          topRight = (updatedGrid !! 0) !! 2
      topLeft `shouldBe` Wall
      topRight `shouldBe` Wall