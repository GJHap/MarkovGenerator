module Main where

import MarkovGenerator

import System.Environment

data CommandLineArgs = CommandLineArgs {filePath :: String} 

parseCommandLine :: [String] -> CommandLineArgs
parseCommandLine ("-f":filePath:_) = CommandLineArgs filePath
parseCommandLine _ = CommandLineArgs ""

usageString :: String
usageString = "Usage:\n-f <filePath>"

printUsage :: IO ()
printUsage = putStrLn usageString

runMarkov :: String -> IO ()
runMarkov filePath = do
    fileStr <- readFile filePath
    let m = generateMarkovChain (words fileStr)
    randomText <- generateMarkovText m
    putStrLn randomText

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> do
            let commandLineArgs = parseCommandLine $ args  
            case filePath commandLineArgs of
                "" -> printUsage
                _ -> runMarkov $ filePath commandLineArgs
        _ -> printUsage
    