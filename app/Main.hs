module Main (main) where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )


main :: IO ()
main = do 
    args <- getArgs
    (_, _) <- parseArgs args
    putStrLn "sorry, yet developed"

parseArgs :: [String] -> IO (FilePath, FilePath)
parseArgs ["-h"] = printUsage >> exitSuccess
parseArgs ["--help"] = printUsage >> exitSuccess
parseArgs ["-v"] = printVersion >> exitSuccess
parseArgs ["--version"] = printVersion >> exitSuccess
parseArgs (inputFile:outputDir:_) = return (inputFile, outputDir)
parseArgs _ = printUsage >> exitFailure

printUsage :: IO ()
printUsage = putStrLn "\
\Usage: simple-parser-generator [option] <input-ebnf> <output-dir> \n\
\This program generates parser programs for grammer written in <input-ebnf> to <output-dir> \n\
\\n\
\[option]\n\
\  -h, --help       print this text \n\
\  -v, --version    print this program's version \n\
\"

printVersion :: IO ()
printVersion = putStrLn "0.0.0"
