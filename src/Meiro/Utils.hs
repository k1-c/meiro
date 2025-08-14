module Meiro.Utils
    ( -- * Shuffling
      fisherYatesShuffle
    , generateRandomIndices
    -- * Time Formatting
    , formatGameTime
    -- * Input/Output Utilities
    , safeGetChar
    ) where

import System.Random (StdGen, randomR)
import Data.Time (NominalDiffTime)
import System.IO (isEOF)
import qualified System.IO as IO
import Control.Exception (catch, IOException)

-- | Fisher-Yates shuffle algorithm for lists
fisherYatesShuffle :: [a] -> StdGen -> ([a], StdGen)
fisherYatesShuffle [] gen = ([], gen)
fisherYatesShuffle xs gen = 
    let len = length xs
        (indices, newGen) = generateRandomIndices len gen
        shuffled = map (xs !!) indices
    in (shuffled, newGen)

-- | Generate a list of random indices for shuffling
generateRandomIndices :: Int -> StdGen -> ([Int], StdGen)
generateRandomIndices 0 gen = ([], gen)
generateRandomIndices n gen = 
    let (idx, gen1) = randomR (0, n-1) gen
        (rest, gen2) = generateRandomIndices (n-1) gen1
        adjustedRest = map (\i -> if i >= idx then i + 1 else i) rest
    in (idx : adjustedRest, gen2)

-- | Format game time in a human-readable format
formatGameTime :: NominalDiffTime -> String
formatGameTime diffTime = 
    let seconds = floor diffTime :: Int
        minutes = seconds `div` 60
        remainingSeconds = seconds `mod` 60
    in if minutes > 0
       then show minutes ++ "m " ++ show remainingSeconds ++ "s"
       else show remainingSeconds ++ "s"

-- | Safe character input with error handling
safeGetChar :: IO Char
safeGetChar = do
    eof <- isEOF
    if eof
    then return 'q'  -- Finish when EOF
    else IO.getChar `catch` handler
  where
    handler :: IOException -> IO Char
    handler _ = return 'q'
