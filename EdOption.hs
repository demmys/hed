module EdOption (parseArgs) where


data Option = OHelp
            | OVersion
            | OScript
            | OPrompt String
            | OFile String
            deriving (Eq, Show)


parseArgs :: [String] -> [Option]
parseArgs [] = []
parseArgs xs = let (option, remains) = getOption xs
                   in option : parseArgs remains

getOption :: [String] -> (Option, [String])
getOption xs@(x:xs') = if isOption x
                           then encodeOption xs
                           else (OFile x, xs')

isOption :: String -> Bool
isOption (c:cs) = if c == '-'
                      then True
                      else False

encodeOption :: [String] -> (Option, [String])
encodeOption (x:xs) = case x of
                          "-h" -> (OHelp, xs)
                          "-v" -> (OVersion, xs)
                          "-p" -> (OPrompt (head xs), tail xs)
                          "-s" -> (OScript, xs)
                          otherwise -> (error ("illigal option " ++ x))
