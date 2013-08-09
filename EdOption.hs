module EdOption (parseArgs, runOption) where


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
                          otherwise -> (error ("illigal option " ++ x))

runOption :: Option -> IO ()
runOption OHelp = putStrLn $ "Usage: ed [options] file\n"
                             ++ "Options:\n"
                             ++ "\t-h\tDisplay this information\n"
                             ++ "\t-v\tDisplay version information\n"
                             ++ "\t-p [string]\tSpecify a command prompt\n"
                             ++ "\t-s\tSuppress diagnostics"
runOption OVersion = putStrLn "haskelled version 0.0.1"
runOption OScript = putStrLn "OScript running"
runOption (OPrompt p) = putStrLn $ "OPrompt running with string " ++ p
runOption (OFile f) = putStrLn $ "OFile running with string " ++ f
