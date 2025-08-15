module Main (main) where

import Meiro.Command (runCommand)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    runCommand args