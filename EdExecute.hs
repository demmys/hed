module EdExecute (execute) where

import System.IO (putStrLn)
import System.Exit (exitSuccess)
import Ed (ed)
import EdOption (EdOption(..))
import EdError (EdError(NO_INPUT_FILE), edError)


execute :: [EdOption] -> IO ()
execute [] = edError NO_INPUT_FILE ""
execute es = exe es ""
    where exe (e:es') p = case e of
                              OError t s -> edError t s
                              OHelp -> edHelp
                              OVersion -> edVersion
                              OScript -> putStrLn "OScript running"
                                         >> exitSuccess
                              OPrompt p' -> exe es' p'
                              OFile f -> ed f p


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
