module Meiro.CommandSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Meiro.Command
import Meiro.Types
import Meiro.MazeGeneration (MazeAlgorithm(..))

spec :: Spec
spec = do
  describe "parseArgs" $ do
    describe "default configuration" $ do
      it "returns default config for empty arguments" $ do
        let config = parseArgs [] defaultConfig
        configWidth config `shouldBe` 30
        configHeight config `shouldBe` 15
        configAlgorithm config `shouldBe` RecursiveBacktrack
        configAction config `shouldBe` PlayGame

    describe "help option" $ do
      it "parses --help" $ do
        let config = parseArgs ["--help"] defaultConfig
        configAction config `shouldBe` ShowHelp

      it "parses -h" $ do
        let config = parseArgs ["-h"] defaultConfig
        configAction config `shouldBe` ShowHelp

      it "ignores other arguments after --help" $ do
        let config = parseArgs ["--help", "--size", "50x50"] defaultConfig
        configAction config `shouldBe` ShowHelp

    describe "version option" $ do
      it "parses --version" $ do
        let config = parseArgs ["--version"] defaultConfig
        configAction config `shouldBe` ShowVersion

      it "parses -v" $ do
        let config = parseArgs ["-v"] defaultConfig
        configAction config `shouldBe` ShowVersion

    describe "size option" $ do
      it "parses --size with valid dimensions" $ do
        let config = parseArgs ["--size", "25x15"] defaultConfig
        configWidth config `shouldBe` 25
        configHeight config `shouldBe` 15
        configAction config `shouldBe` PlayGame

      it "parses -s with valid dimensions" $ do
        let config = parseArgs ["-s", "35x25"] defaultConfig
        configWidth config `shouldBe` 35
        configHeight config `shouldBe` 25

      it "rejects even width" $ do
        let config = parseArgs ["--size", "24x15"] defaultConfig
        case configAction config of
          InvalidArgs msg -> msg `shouldContain` "Invalid size format"
          _ -> expectationFailure "Expected InvalidArgs"

      it "rejects even height" $ do
        let config = parseArgs ["--size", "25x14"] defaultConfig
        case configAction config of
          InvalidArgs _ -> return ()
          _ -> expectationFailure "Expected InvalidArgs"

      it "rejects size less than 5" $ do
        let config = parseArgs ["--size", "3x5"] defaultConfig
        case configAction config of
          InvalidArgs _ -> return ()
          _ -> expectationFailure "Expected InvalidArgs"

      it "rejects invalid format" $ do
        let config = parseArgs ["--size", "invalid"] defaultConfig
        case configAction config of
          InvalidArgs _ -> return ()
          _ -> expectationFailure "Expected InvalidArgs"

    describe "algorithm option" $ do
      it "parses --algorithm recursive" $ do
        let config = parseArgs ["--algorithm", "recursive"] defaultConfig
        configAlgorithm config `shouldBe` RecursiveBacktrack

      it "parses --algorithm kruskal" $ do
        let config = parseArgs ["--algorithm", "kruskal"] defaultConfig
        configAlgorithm config `shouldBe` Kruskal

      it "parses --algorithm prim" $ do
        let config = parseArgs ["--algorithm", "prim"] defaultConfig
        configAlgorithm config `shouldBe` Prim

      it "parses -a with algorithms" $ do
        let config = parseArgs ["-a", "kruskal"] defaultConfig
        configAlgorithm config `shouldBe` Kruskal

      it "rejects unknown algorithm" $ do
        let config = parseArgs ["--algorithm", "unknown"] defaultConfig
        case configAction config of
          InvalidArgs msg -> msg `shouldContain` "Unknown algorithm"
          _ -> expectationFailure "Expected InvalidArgs"

    describe "combined options" $ do
      it "parses size and algorithm together" $ do
        let config = parseArgs ["--size", "25x15", "--algorithm", "prim"] defaultConfig
        configWidth config `shouldBe` 25
        configHeight config `shouldBe` 15
        configAlgorithm config `shouldBe` Prim
        configAction config `shouldBe` PlayGame

      it "parses short forms together" $ do
        let config = parseArgs ["-s", "35x25", "-a", "kruskal"] defaultConfig
        configWidth config `shouldBe` 35
        configHeight config `shouldBe` 25
        configAlgorithm config `shouldBe` Kruskal

    describe "invalid arguments" $ do
      it "rejects unknown options" $ do
        let config = parseArgs ["--unknown"] defaultConfig
        case configAction config of
          InvalidArgs msg -> msg `shouldContain` "Unknown option"
          _ -> expectationFailure "Expected InvalidArgs"

  describe "parseSize" $ do
    it "parses valid size strings" $ do
      parseSize "25x15" `shouldBe` Just (25, 15)
      parseSize "5x5" `shouldBe` Just (5, 5)
      parseSize "101x51" `shouldBe` Just (101, 51)

    it "rejects invalid formats" $ do
      parseSize "25" `shouldBe` Nothing
      parseSize "25y15" `shouldBe` Nothing
      parseSize "invalid" `shouldBe` Nothing
      parseSize "" `shouldBe` Nothing

    it "rejects even numbers" $ do
      parseSize "24x15" `shouldBe` Nothing
      parseSize "25x14" `shouldBe` Nothing
      parseSize "24x14" `shouldBe` Nothing

    it "rejects sizes less than 5" $ do
      parseSize "3x5" `shouldBe` Nothing
      parseSize "5x3" `shouldBe` Nothing
      parseSize "1x1" `shouldBe` Nothing

  describe "parseAlgorithm" $ do
    it "parses valid algorithm names" $ do
      parseAlgorithm "recursive" `shouldBe` Just RecursiveBacktrack
      parseAlgorithm "kruskal" `shouldBe` Just Kruskal
      parseAlgorithm "prim" `shouldBe` Just Prim

    it "rejects invalid algorithm names" $ do
      parseAlgorithm "unknown" `shouldBe` Nothing
      parseAlgorithm "RECURSIVE" `shouldBe` Nothing  -- case sensitive
      parseAlgorithm "" `shouldBe` Nothing

  describe "edge cases" $ do
    it "handles mixed valid and invalid arguments gracefully" $ do
      let config1 = parseArgs ["--size", "25x15", "--unknown"] defaultConfig
      case configAction config1 of
        InvalidArgs _ -> return ()
        _ -> expectationFailure "Expected InvalidArgs for unknown option"
      
      let config2 = parseArgs ["--algorithm", "recursive", "--size", "24x15"] defaultConfig
      case configAction config2 of
        InvalidArgs _ -> return ()
        _ -> expectationFailure "Expected InvalidArgs for invalid size"