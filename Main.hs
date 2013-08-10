module Main where

import System.Environment (getArgs)
import EdOption (parseArgs)
import EdExecute (execute)

main = do args <- getArgs
          let options = parseArgs args
          execute options
