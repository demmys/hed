module EdExecute (execute) where

import System.Exit (exitSuccess)
import System.IO
import Data.List (sort)
import EdOption (Option(..))
import EdError (EdError(..), edError)


execute :: [Option] -> IO ()
execute [] = edError NO_INPUT_FILE ""
execute es = exe es ""
    where exe (e:es') p = case e of
                              OError t s -> edError t s
                              OHelp -> edHelp
                              OVersion -> edVersion
                              OScript -> putStrLn "OScript running"
                                         >> exitSuccess
                              OPrompt p' -> exe es' p'
                              OFile f -> do h <- openFile f ReadWriteMode
                                            str <- hGetContents h
                                            putStrLn $ show (length str)
                                            ed p h


edHelp = (putStrLn $ "Usage: ed [options] file\n"
                     ++ "Options:\n"
                     ++ "\t-h\tDisplay this information\n"
                     ++ "\t-v\tDisplay version information\n"
                     ++ "\t-p [string]\tSpecify a command prompt\n"
                     ++ "\t-s\tSuppress diagnostics")
         >> exitSuccess

edVersion = (putStrLn $ "hed\n"
                        ++ " -- line text editor implemented in Haskell\n"
                        ++ "version 0.0.1")
            >> exitSuccess


ed :: String -> Handle -> IO ()
ed p h = do putStr p
            hFlush stdout
            cmd <- getLine
            parseCommand h cmd
            ed p h


parseCommand :: Handle -> String -> IO ()
parseCommand _ "q" = exitSuccess
parseCommand _ _ = do putStrLn "?"
