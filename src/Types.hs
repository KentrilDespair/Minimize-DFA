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
               } deriving (Eq)

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


-- Takes all the remaning elements from a certain index
takeFrom :: Int -> [a] -> [a]
takeFrom _ [] = []
takeFrom n all@(x:xs)
    | n <= 0 = all
    | otherwise = takeFrom (n-1) xs

-- *> strToList "1,2,3,4" :: [Int]
strToList :: (Read a) => String -> [a]
strToList s = read $ '[' : s ++ "]"

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy d str = prev : if null rest then []
                                    else splitBy d (tail rest)
    where (prev, rest) = span (/= d) str

readTrans :: String -> Trans
readTrans s = (src, symb, dst)
    where strans = splitBy ',' s
          src = read $ strans !! 0 :: State
          symb = read $ '\'' : strans !! 1 ++ "'" :: Char
          dst = read $ strans !! 2 :: State


instance Read DFA where
    readsPrec _ s = 
        let inList = lines s
            states = strToList (inList !! 0) :: [State]
            alpha = inList !! 1
            init = read (inList !! 2) :: State
            final = strToList (inList !! 3) :: [State]
            trans = map readTrans (takeFrom 4 inList)
        in (\s -> [(DFA states alpha init final trans ,"")]) s




