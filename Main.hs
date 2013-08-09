module Main where

import System.Environment (getArgs)
import EdOption (parseArgs, runOption)

main = do args <- getArgs
          let options = parseArgs args
          mapM_ runOption options
