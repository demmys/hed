module EdError (EdError(..), edError) where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

data EdError = NO_INPUT_FILE
             | ILLIGAL_OPTION
             deriving (Show, Eq)

edError :: EdError -> String -> IO ()
edError t s = do edErrorMessage t s
                 exitFailure
    where edErrorMessage NO_INPUT_FILE _ = do hPutStrLn stderr "no input file"
          edErrorMessage ILLIGAL_OPTION s = do hPutStrLn stderr ("illigal option " ++ s)
