module Main where

import System.Environment

import MarkovGenerator

main = do
    args <- getArgs
    fileStr <- readFile $ args !! 0
    let m = generateMap (words fileStr)
    randomText <- generateMarkovText m
    putStrLn randomText
    