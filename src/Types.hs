{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Types module
 -  TODO
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards #-}

module Types where
-- TODO

import Data.List (intercalate)

-- State of DFA as an Integer (non-negative TODO)
type State = Int

-- Symbol of the input alphabet
type Symbol = Char

-- Transition from state 'src' to state 'dst' using symbol 'symb'
type Trans = (State, Symbol, State)

-- Deterministic Finite Automaton consists of 
--  'states' - a finite set of states 
--  'alphabet' - a finite set of input symbols
--  'initial' - an initial state
--  'final' - a set of final states
--  'trans' - a set of transitions
data DFA = DFA { states   :: [State]
               , alphabet :: [Symbol]
               , initial  :: State
               , final    :: [State]
               , trans    :: [Trans]
               } deriving (Eq, Read)

-- Output in format:
--  states separated by comma
--  symbols of the alphabet
--  initial state
--  final states
--  transition rules as triples, one per line
instance Show DFA where
    show DFA{..} = unlines $ [showList states,
                              alphabet,
                              show initial,
                              showList final
                             ] ++ (map showTrans trans)
        where showList = intercalate "," . map show
              showTrans (p, a, q) = show p ++ [',', a, ','] ++ show q






