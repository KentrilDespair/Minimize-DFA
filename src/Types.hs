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
import Data.Set (Set, toList)

-- | An error message
type Error = String

-- | State of DFA as an Integer (non-negative TODO)
type State = Int

-- | A finite list of states
type States = Set State

-- | A pair of states, i.e. states in a relation
type StatePair = (State, State)

-- | Symbol of the input alphabet
type Symbol = Char

-- | A finit list of symbols
type Alphabet = Set Symbol

-- | A transition rule is a triple, i.e.:
-- | transition from state 'src' to state 'dst' using symbol 'symb'
type Trans = (State, Symbol, State)

-- TODO Used?
transRule :: State -> Symbol -> State -> Trans
transRule p a q = (p, a, q)

transSrc :: Trans -> State
transSrc (s, _, _) = s

transSymb :: Trans -> Symbol
transSymb (_, a, _) = a

transDst :: Trans -> State
transDst (_, _, s) = s

-- | A finite set of transition rules
type TransRules = Set Trans

-- | Deterministic Finite Automaton consists of 
-- | 'states':   a finite, non-empty set of states 
-- | 'alphabet': a finite, non-empty set of input symbols
-- | 'initial':  an initial state
-- | 'final':    a set of final states
-- | 'trans':    a set of transitions
data DFA = DFA { states   :: States
               , alpha    :: Alphabet
               , init     :: State
               , final    :: States
               , trans    :: TransRules
               } deriving (Eq)

-- | List accessors for e.g. list comprehensions
statesList :: DFA -> [State]
statesList DFA{..} = toList states

alphaList :: DFA -> [Symbol]
alphaList DFA{..} = toList alpha 

finalList :: DFA -> [State]
finalList DFA{..} = toList final 

transList :: DFA -> [Trans]
transList DFA{..} = toList trans 

-- Output in format:
--  states separated by comma
--  symbols of the alphabet
--  initial state
--  final states
--  transition rules as triples, one per line
instance Show DFA where
    show DFA{..} = unlines $ [showList states,
                              toList alpha,
                              show init,
                              showList final
                             ] ++ (map showTrans $ toList trans)
        where showList = intercalate "," . map show . toList
              showTrans (p, a, q) = show p ++ [',', a, ','] ++ show q



