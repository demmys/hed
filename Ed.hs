module Ed (ed) where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit)

ed :: String -> Handle -> IO ()
ed p h = running 1 p h
    where running n p h = do putStr p
                             hFlush stdout
                             cmd <- getLine
                             n' <- parseCommand n h cmd
                             running n' p h


parseCommand :: Integer -> Handle -> String -> IO Integer
parseCommand _ h "q" = hClose h >> exitSuccess
parseCommand n _ ".=" = putStrLn (show n) >> return n
parseCommand n h s | isDigital s = return (read s :: Integer)
parseCommand n _ _ = putStrLn "?" >> return n

isDigital = and . map isDigit
