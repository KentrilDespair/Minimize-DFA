{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Main module
 ------------------------------------------------------------------------------}

module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile)

import Types



dispatch :: [(String, [String] -> IO [()])]
dispatch = [ ("-i", printDFA)
           , ("-t", minimizeDFA)
           ]

main :: IO ()
main = do
    args <- getArgs 
    procArgs args
    return ()

    --contents <- getContents
    ----let sargs = unlines args
    --let lineContents = lines contents
    --let action = lookup cmd dispatch
    --case action of
    --    Just ac -> ac lineContents
    --    Nothing -> error "Error: unknown command."

    --let dfa = DFA [1,2,3] "abc" 1 [3] [(1,'a',2),(2,'b',3),(2,'c',1)]
    --let dfa = read contents :: DFA
    --putStrLn ("Contents:\n" ++ contents)
    --mapM putStrLn (["Args:"] ++ args ++ ["-----------------"])
    --print dfa

procArgs :: [String] -> IO [()]
procArgs [] = do die ("Error: No arguments supplied") -- TODO help
procArgs (opt:inputName) = do
    contents <- getInputContents inputName
    let linesContents = lines contents
    case opt of
        "-i" -> printDFA linesContents 
        "-t" -> minimizeDFA linesContents 
        _    -> die ("Error: Unknown arguments")
        -- TODO help

getInputContents :: [String] -> IO String
getInputContents input = do
    case input of 
        []  -> getContents
        [x] -> readFile x
        _   -> die ("Error: incorrect number of arguments")
        -- TODO help


printDFA :: [String] -> IO [()]
printDFA ss = mapM putStrLn (["printDFA"] ++ ss)

minimizeDFA :: [String] -> IO [()]
minimizeDFA ss = mapM putStrLn (["minimizeDFA"] ++ ss)

