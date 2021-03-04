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

import Types

import System.Environment (getArgs)


main = do
    contents <- getContents
    args <- getArgs
    --let sargs = unlines args
    let lineContents = lines contents
    --let dfa = DFA [1,2,3] "abc" 1 [3] [(1,'a',2),(2,'b',3),(2,'c',1)]
    let dfa = read contents :: DFA
    putStrLn ("Contents:\n" ++ contents)
    mapM putStrLn (["Args:"] ++ args ++ ["-----------------"])
    print dfa



