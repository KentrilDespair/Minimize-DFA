{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Module for parsing input string into valid DFA representation.
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards #-}

module ParseInput 
( parseDFA
) where

import Types (Error, State, States, Symbol, Alphabet, Trans, TransRules, DFA(..), 
              transRule, transSrc, transSymb, transDst) 

import Data.Char (isAsciiLower)
import Data.Set (fromList, toList, member, isSubsetOf)

import Text.Parsec.String (Parser)
import Text.Parsec (parse, satisfy, newline, 
                    sepBy1, sepBy, many1, endBy,
                    digit, char)
import Control.Monad ((<=<))
import Control.Arrow (left)
import Control.Applicative ((<$>), (<*>), (<$), (<*), (<|>))


-- | Converts input string to DFA. Return the DFA is syntactically correct else
-- |    returns an error
parseDFA :: String -> Either Error DFA
parseDFA = isDFA <=< left show . parse parserDFA ""
    where parserDFA = DFA <$> parseStates       <* newline
                          <*> parseAlphabet     <* newline
                          <*> parseState        <* newline 
                          <*> parseFinalStates  <* newline
                          <*> parseTransRules

-- | States are non-empty, finite set of states separated by comma
parseStates :: Parser States
parseStates = fromList <$> parseState `sepBy1` sepComma

-- | State is a non-negative integer
parseState :: Parser State
parseState = read <$> many1 digit

-- | Alphabet is non-empty, finite set symbols, written in a string
parseAlphabet :: Parser Alphabet
parseAlphabet = fromList <$> many1 parseSymbol

-- | Symbol is a lower case character from a set [a-z]
parseSymbol :: Parser Symbol
parseSymbol = satisfy isAsciiLower

-- | Final states is finite set of states
parseFinalStates :: Parser States
parseFinalStates = fromList <$> parseState `sepBy` sepComma

-- | Optional finite set of transition rules
parseTransRules :: Parser TransRules
parseTransRules = fromList <$> parseTrans `endBy` newline

-- | Transition rule is a triple 'from State, using Symbol, to State'
-- | each separated by comma
parseTrans :: Parser Trans
parseTrans = transRule <$> parseState <* sepComma <*> parseSymbol <* sepComma
                       <*> parseState

-- | Comma character separator
sepComma :: Parser Char 
sepComma = char ','

-- | Syntax checks if is Deterministic Finite Automaton
-- |    initial state is in states
-- |    final states are in states
-- |    for each transition rule: 
-- |        each state is in states
-- |        each symbol is in alphabet
-- |    for each state:
-- |        has deterministic transition rules
isDFA :: DFA -> Either Error DFA
isDFA dfa@DFA{..}
    | not isFA    = Left "Syntactically incorrect Finite Automaton"
    | isNotDFA    = Left "Non-deterministic Finite Automaton"
    | otherwise   = Right dfa
    where 
        isFA       = initial `member` states
                  && final `isSubsetOf` states
                  && all (inStates . transSrc) ts
                  && all (inAlphabet . transSymb) ts 
                  && all (inStates . transDst) ts
        inStates   = (`member` states)
        inAlphabet = (`member` alpha)
        isNotDFA   = isNFA ts
        ts         = toList trans

-- | Is non-deterministic FA if only one transition rule is ambiguous
-- |    assumes no duplicate transition rules (is from Set)
isNFA :: [Trans] -> Bool
isNFA [] = False
isNFA (t:ts) = isTransAmbiguous t ts || isNFA ts

-- | Transition is ambiguous if from the same state gets into 
-- |    other states using the same symbol
isTransAmbiguous :: Trans -> [Trans] -> Bool
isTransAmbiguous _ [] = False
isTransAmbiguous x (t:ts) = isSameSrcSymb x t || isTransAmbiguous x ts
    where isSameSrcSymb (q, a, _) (p, c, _) = q == p && a == c


