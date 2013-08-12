module EdCommand (EdCommand(..), parseCommand) where

import Data.Char (isDigit, digitToInt, isSpace)

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
               | CSearch String
               | CBSearch String
               | CSubstitute [String]
               | CUndo
               | CShellCommand [String]
               | CError
               deriving (Show, Eq)

parseCommand :: [Char] -> [EdCommand]
parseCommand [] = []
parseCommand cs = let (cmd, rem) = getCommand cs
                  in cmd : parseCommand rem

getCommand :: [Char] -> (EdCommand, [Char])
getCommand cs = let (n, rem) = takeWhileDigit cs
                    in if n > 0
                           then (CMove n, rem)
                           else encode cs

encode :: [Char] -> (EdCommand, [Char])
encode cs@(c:cs') = case c of
                        'q' -> (CQuit, [])
                        'w' -> if isSpace (head cs')
                                   then (CWrite (dropWhile isSpace cs'), [])
                                   else (CWrite "", cs')
                        'Q' -> (CForceQuit, [])
                        '.' -> (CCurrent, cs')
                        '=' -> (CLineNumber, cs')
                        'p' -> (CPrint, cs')
                        '+' -> let (n, rem) = takeWhileDigit cs'
                               in (CInc n, rem)
                        '-' -> let (n, rem) = takeWhileDigit cs'
                               in (CDec n, rem)
                        ',' -> (CTo, cs')
                        '$' -> (CLast, cs')
                        'a' -> (CAppend, cs')
                        'i' -> (CInsert, cs')
                        'c' -> (CChange, cs')
                        'd' -> (CDelete, cs')
                        '/' -> (CSearch cs', [])
                        '?' -> (CBSearch cs', [])
                        's' -> let ss = split cs' '/'
                               in if length ss >= 2
                                      then (CSubstitute (take 3 ss), [])
                                      else (CError, [])
                        'u' -> (CUndo, [])
                        '!' -> (CShellCommand (words cs'), [])
                        otherwise -> (CError, [])

split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split (x:xs) t = if x == t
                     then split xs t
                     else (x : takeWhile (/= t) xs) : split (dropWhile (/= t) xs) t


takeWhileDigit :: String -> (Int, String)
takeWhileDigit [] = (0, [])
takeWhileDigit xs = separate (0, xs)
    where separate (n, []) = (n, [])
          separate (n, xs@(x:xs')) =
              if isDigit x
                  then separate (n * 10 + digitToInt x, xs')
                  else (n, xs)
