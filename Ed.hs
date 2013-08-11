module Ed (ed) where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit, digitToInt)

data EdCommand = CQuit
               | CWrite
               | CForceQuit
               | CCurrent
               | CNumber
               | CLine Int
               | CPrint
               | CInc Int
               | CDec Int
               | CAll
               | CLast
               | CAppend
               | CInsert
               | CChange
               | CDelete
               | CFSearch
               | CBSearch
               | CSubstitute
               | CUndo
               | CShellCommand
               deriving (Show, Eq)

type Buffer = [String]
data EdEnvironment = EdEnvironment FilePath Buffer Int
                   deriving (Show, Eq)

setBuffer :: EdEnvironment -> Buffer -> EdEnvironment
setBuffer (EdEnvironment f b n) b' = EdEnvironment f b' n


-- TODO コマンド型配列を適用
apply :: EdEnvironment -> EdCommand -> (EdEnvironment, [String])
apply e c = undefined


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
applyCommand e (c:cs) = let (e', ls) = apply e c
                              in if null ls
                                     then return e'
                                     else applyCommand (setBuffer e' ls) cs


--TODO コマンド文字列を適切なコマンド型配列に変換
-- 不正なコマンド型配列になる場合はエラー
parseCommand :: String -> [EdCommand]
parseCommand cs = undefined


isDigital = and . map isDigit

takeWhileDigit :: String -> (Int, String)
takeWhileDigit [] = (0, [])
takeWhileDigit xs = separate (0, xs)
    where separate (n, []) = (n, [])
          separate (n, xs@(x:xs')) =
              if isDigit x
                  then separate (n * 10 + digitToInt x, xs')
                  else (n, xs)
