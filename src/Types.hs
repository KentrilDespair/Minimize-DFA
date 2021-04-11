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
import Data.Map (Map, empty, insert, (!))


-- | An error message
type Error = String

-- | State of DFA as an integer (non-negative)
type State = Int

-- | A finite set of states
type States = Set State

-- | A pair of states, i.e. states in a relation
type StatePair = (State, State)

-- | Symbol of the input alphabet
type Symbol = Char

-- | A finite set of symbols
type Alphabet = Set Symbol

-- | A transition rule is a triple:
-- | transition from state 'src' using symbol 'symb' to 'dst' state
-- |          'src'  'symb'  'dst'
type Trans = (State, Symbol, State)

-- | A finite set of transition rules
type TransRules = Set Trans

-- | Transition "constructor" and accessors
transRule :: State -> Symbol -> State -> Trans
transRule p a q = (p, a, q)

transSrc :: Trans -> State
transSrc (s, _, _) = s

transSymb :: Trans -> Symbol
transSymb (_, a, _) = a

transDst :: Trans -> State
transDst (_, _, s) = s

-- | Deterministic Finite Automaton consists of 
-- | 'states':   a finite, non-empty set of states 
-- | 'alphabet': a finite, non-empty set of input symbols
-- | 'inital':  an initial state
-- | 'final':    a set of final states
-- | 'trans':    a set of transitions
data DFA = DFA { states   :: States
               , alpha    :: Alphabet
               , init     :: State
               , final    :: States
               , trans    :: TransRules
               } deriving (Eq)

-- | List accessors for e.g. list comprehensions
stateList :: DFA -> [State]
stateList DFA{..} = toList states

alphaList :: DFA -> [Symbol]
alphaList DFA{..} = toList alpha 

finalList :: DFA -> [State]
finalList DFA{..} = toList final 

transList :: DFA -> [Trans]
transList DFA{..} = toList trans 

-- | Proper transition function
type TransFnc = Map (State, Symbol) State

toTransFnc :: TransRules -> TransFnc
toTransFnc = foldl (\macc (p,a,q) -> insert (p,a) q macc) empty

toTransFncL :: [Trans] -> TransFnc
toTransFncL = foldl (\macc (p,a,q) -> insert (p,a) q macc) empty

-- | Returns the destination state we get into from a state using a symbol
-- |    Expects fully defined transition function
getDest :: (State, Symbol) -> TransFnc -> State
getDest k m = m ! k


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
                             ] ++ map showTrans (toList trans)
        where showList = intercalate "," . map show . toList
              showTrans (p, a, q) = show p ++ [',', a, ','] ++ show q



