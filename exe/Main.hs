module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec
import Data.List (partition)
import Data.Char (isSpace)
import Control.Monad

import Parser
import Analyzer
import Transpiler

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> compileFile inputFile outputFile
        _ -> do
            putStrLn "Usage: wic <input.wic> <output.c>"
            exitFailure

compileFile :: String -> String -> IO ()
compileFile inputFile outputFile = do
    content <- readFile inputFile

    let sourceLines = lines content
        isDirective line = case dropWhile isSpace line of
            ('#':_) -> True
            _       -> False
        (directives, codeLines) = partition isDirective sourceLines
        cleanContent = unlines codeLines

    -- 2. Parse the Cleaned Source File
    let parseResult = parse (sc *> pFile <* eof) inputFile cleanContent
    ast <- case parseResult of
        Left errBundle -> do
            putStrLn "Syntax Error:"
            putStrLn (errorBundlePretty errBundle)
            exitFailure
        Right tree -> return tree

    -- 3. Analyze Compile-Time Invariants
    (errors, unknowns) <- runAnalyzer ast
    
    if null errors
        then do
            
            unless (null unknowns) $ putStrLn $ "WARNING:  " ++ show (length unknowns) ++ " unprovable constraints found."
            
            let cCode = transpile directives unknowns ast
            writeFile outputFile cCode
            
            exitSuccess
        else do
            putStrLn "ERROR: found provably false states/contracts:"
            mapM_ (\e -> putStrLn $ "  -> " ++ e) errors
            exitFailure