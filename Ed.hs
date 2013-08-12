module Ed (ed) where

import System.Exit (exitSuccess)
import System.IO
import EdCommand (EdCommand(..), parseCommand)

type Buffer = [String]
data EdEnvironment = EdEnvironment {
                         getFilePath :: FilePath,
                         getBuffer :: Buffer,
                         getLineNo :: Int
                     }deriving (Show, Eq)

setBuffer :: EdEnvironment -> Buffer -> EdEnvironment
setBuffer (EdEnvironment f b n) b' = EdEnvironment f b' n


ed :: FilePath -> String -> IO ()
ed f p = do contents <- readFile f
            let buf = lines contents
            putStrLn $ show (length buf)
            run p $ EdEnvironment f buf 1
    where run p e = do putStr p
                       hFlush stdout
                       cmd <- getLine
                       let cs = parseCommand cmd
                       e' <- applyCommand e cs
                       run p e'



applyCommand :: EdEnvironment -> [EdCommand] -> IO EdEnvironment
applyCommand e [] = return e
applyCommand e (c:cs) = do (e', ls) <- apply e c
                           if null ls
                               then return e'
                               else applyCommand (setBuffer e' ls) cs

-- TODO コマンド型配列を適用
apply :: EdEnvironment -> EdCommand -> IO (EdEnvironment, [String])
apply e c = case c of
                CQuit -> exitSuccess -- TODO check if buf saved
                CForceQuit -> exitSuccess
                CError -> putStrLn "?" >> return (e, [])
                otherwise -> putStrLn "?" >> return (e, [])
