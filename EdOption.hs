module EdOption (Option(..), parseArgs) where

import EdError (EdError(ILLIGAL_OPTION))

data Option = OHelp
            | OVersion
            | OScript
            | OPrompt String
            | OFile String
            | OError EdError String
            deriving (Show, Eq)


parseArgs :: [String] -> [Option]
parseArgs [] = []
parseArgs xs = let (opt, rem) = getOption xs
                   in opt : parseArgs rem


getOption :: [String] -> (Option, [String])
getOption xs@(x:xs') = if isOption x
                           then encode xs
                           else (OFile x, xs')


isOption :: String -> Bool
isOption (c:cs) = if c == '-'
                      then True
                      else False


encode :: [String] -> (Option, [String])
encode (x:xs) = case x of
                    "-h" -> (OHelp, xs)
                    "-v" -> (OVersion, xs)
                    "-p" -> (OPrompt (head xs), tail xs)
                    "-s" -> (OScript, xs)
                    otherwise -> (OError ILLIGAL_OPTION x, [])
