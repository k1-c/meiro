module Meiro.UtilsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import System.Random (mkStdGen)
import Data.Time (secondsToNominalDiffTime)
import Meiro.Utils

spec :: Spec
spec = describe "Meiro.Utils" $ do
  
  describe "Fisher-Yates shuffle" $ do
    it "preserves list length" $ property $ 
      \xs -> let (shuffled, _) = fisherYatesShuffle xs (mkStdGen 42)
             in length shuffled == length (xs :: [Int])
    
    it "preserves list elements" $ property $
      \xs -> let (shuffled, _) = fisherYatesShuffle xs (mkStdGen 42)
             in sort shuffled == sort (xs :: [Int])
    
    it "handles empty lists" $ do
      let (result, _) = fisherYatesShuffle ([] :: [Int]) (mkStdGen 42)
      result `shouldBe` []
    
    it "handles single element lists" $ do
      let (result, _) = fisherYatesShuffle [42] (mkStdGen 42)
      result `shouldBe` [42]

  describe "Random indices generation" $ do
    it "generates correct number of indices" $ property $
      \(Positive n) -> let (indices, _) = generateRandomIndices n (mkStdGen 42)
                       in length indices == n
    
    it "generates unique indices" $ property $
      \(Positive n) -> let (indices, _) = generateRandomIndices n (mkStdGen 42)
                       in length indices == length (nub indices)
    
    it "generates indices in valid range" $ property $
      \(Positive n) -> let (indices, _) = generateRandomIndices n (mkStdGen 42)
                       in all (\i -> i >= 0 && i < n) indices

  describe "Time formatting" $ do
    it "formats seconds correctly" $ do
      let time = secondsToNominalDiffTime 45
      formatGameTime time `shouldBe` "45s"
    
    it "formats minutes and seconds" $ do
      let time = secondsToNominalDiffTime 125  -- 2m 5s
      formatGameTime time `shouldBe` "2m 5s"
    
    it "handles exact minutes" $ do
      let time = secondsToNominalDiffTime 120  -- 2m 0s  
      formatGameTime time `shouldBe` "2m 0s"
    
    it "handles zero time" $ do
      let time = secondsToNominalDiffTime 0
      formatGameTime time `shouldBe` "0s"

-- Helper functions
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger
  where
    smaller = filter (< x) xs
    larger = filter (>= x) xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)