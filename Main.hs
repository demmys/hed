module Main where

import System.Environment (getArgs)
import EdOption (parseArgs)

main = do args <- getArgs
          let options = parseArgs args
          putStrLn $ show options
