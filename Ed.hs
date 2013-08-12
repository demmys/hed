module Ed (ed) where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit, digitToInt)

data EdCommand = CQuit
               | CWrite FilePath
               | CForceQuit
               | CCurrent
               | CLineNumber
               | CMove Int
               | CPrint
               | CInc Int
               | CDec Int
               | CTo
               | CLast
               | CAppend
               | CInsert
               | CChange
               | CDelete
               | CFSearch String
               | CBSearch String
               | CSubstitute String String [Char]
               | CUndo
               | CShellCommand [String]
               | CError
               deriving (Show, Eq)

type Buffer = [String]
data EdEnvironment = EdEnvironment FilePath Buffer Int
                   deriving (Show, Eq)

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


parseCommand :: String -> [EdCommand]
parseCommand cs@(c:cs') = if isCommand c
                              then let (cmd, rem) = encode cs
                                   in cmd : parseCommand rem
                              else [CError]

--TODO コマンド文字列を適切なコマンド型配列に変換
encode :: String -> (EdCommand, String)
encode cs = undefined

isCommand :: Char -> Bool
isCommand = flip elem "qwQ.=123456789p+-,$aicd/?su!"


applyCommand :: EdEnvironment -> [EdCommand] -> IO EdEnvironment
applyCommand e [] = return e
applyCommand e (c:cs) = do (e', ls) <- apply e c
                           if null ls
                               then return e'
                               else applyCommand (setBuffer e' ls) cs


-- TODO コマンド型配列を適用
apply :: EdEnvironment -> EdCommand -> IO (EdEnvironment, [String])
apply e c = undefined


isDigital = and . map isDigit

takeWhileDigit :: String -> (Int, String)
takeWhileDigit [] = (0, [])
takeWhileDigit xs = separate (0, xs)
    where separate (n, []) = (n, [])
          separate (n, xs@(x:xs')) =
              if isDigit x
                  then separate (n * 10 + digitToInt x, xs')
                  else (n, xs)
