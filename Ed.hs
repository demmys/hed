module Ed (ed) where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit)

ed :: String -> [String] -> IO ()
ed p s = running 1 p s
    where running n p s = do putStr p
                             hFlush stdout
                             cmd <- getLine
                             n' <- parseCommand n s cmd
                             running n' p s


parseCommand :: Int -> [String] -> String -> IO Int
parseCommand n _ [] = return n
parseCommand n s ns@(c:cs) =
    if isDigital ns
        then parseCommand (read ns :: Int) s "p"
        else case c of
                 'q' -> exitSuccess
                 '.' -> if null cs
                            then parseCommand n s "p"
                            else parseCommand n s cs
                 'p' -> putStrLn (s !! (n - 1)) >> return n
                 '=' -> putStrLn (show n) >> return n
                 otherwise -> putStrLn "?" >> return n
{-
parseCommand _ h "q" = hClose h >> exitSuccess
parseCommand n _ ".=" = putStrLn (show n) >> return n
parseCommand n _ _ = putStrLn "?" >> return n
-}

isDigital = and . map isDigit
