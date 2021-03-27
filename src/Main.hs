{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Main module: argument parsing, input reading, and selecting the appropriate
 -  action.
 ------------------------------------------------------------------------------}

module Main (main) where

import Types (DFA)
import ParseInput (parseDFA)
import Minimize (minimizeDFA)

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile)


-- | Printed on any error related to command line arguments
usageMsg :: String
usageMsg = unlines [
    "Usage: dka-2-mka option [input]",
    " Option: ",
    "   -i    Parses the input DFA, then prints to STDOUT if syntactically correct.",
    "   -t    Prints the minimized DFA to STDOUT if syntactically correct.",
    "   input Name of the input file, else STDIN is used"
    ]


-- | A selected action based on arguments is then applied on the parsed input DFA
main :: IO ()
main = do
    args            <- getArgs 
    (action, input) <- procArgs args
    either die action (parseDFA input)

-- | Processes arguments, returns action to be taken and the input
procArgs :: [String] -> IO (DFA -> IO (), String)
procArgs [] = die usageMsg
procArgs (opt:inputName) = do
    action   <- getAction opt
    contents <- getInputContents inputName
    return (action, contents)

-- | Selects the appropriate action based on the option
getAction :: String -> IO (DFA -> IO ())
getAction opt =
    case opt of
        "-i" -> return printDFA
        "-t" -> return printMDFA
        _    -> die usageMsg

-- | Based on options either reads the input from a file or STDIN
getInputContents :: [String] -> IO String
getInputContents input =
    case input of 
        []  -> getContents
        [x] -> readFile x
        _   -> die usageMsg

-- | Prints the internal representation of the syntactically correct input DFA
printDFA :: DFA -> IO ()
printDFA = putStr . show

-- | Prints the minimized DFA to STDOUT
printMDFA :: DFA -> IO ()
printMDFA = putStr . show . minimizeDFA


