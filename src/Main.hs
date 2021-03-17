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

-- TODO what is imported
import Types
import ParseInput
import Minimize (minimizeDFA)

usageMsg :: String
usageMsg = unlines [
    "Usage: dka-2-mka option [input]",
    " Option: ",
    "   -i Parses the input DFA, then prints to STDOUT if syntactically correct.",
    "   -t Prints the minimized DFA to STDOUT if syntactically correct.",
    "   input Name of the input file, else STDOUT is used"
    ]

main :: IO ()
main = do
    args <- getArgs 
    (action, input) <- procArgs args
    either die action (parseDFA input)

-- | Processes arguments, returns input and action to be taken
procArgs :: [String] -> IO (DFA -> IO (), String)
procArgs [] = do die usageMsg
procArgs (opt:inputName) = do
    contents <- getInputContents inputName
    case opt of
        "-i" -> return (printDFA, contents)
        "-t" -> return (printMDFA, contents) 
        _    -> die "Error: Unknown option"
        -- TODO help

-- | 
getInputContents :: [String] -> IO String
getInputContents input = do
    case input of 
        []  -> getContents
        [x] -> readFile x
        _   -> die "Error: incorrect number of options"

printDFA :: DFA -> IO ()
printDFA = putStr . show

printMDFA :: DFA -> IO ()
printMDFA = putStr . show . minimizeDFA

