module Ed (ed) where

import System.IO
import System.Exit (exitSuccess)

ed :: String -> Handle -> IO ()
ed p h = do putStr p
            hFlush stdout
            cmd <- getLine
            parseCommand h cmd
            ed p h


parseCommand :: Handle -> String -> IO ()
parseCommand _ "q" = exitSuccess
parseCommand _ _ = do putStrLn "?"
