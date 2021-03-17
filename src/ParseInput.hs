{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Module for parsing input 
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards #-}

module ParseInput 
( parseDFA
) where

import Types 
--(Error, DFA, State, States, 
             -- Alphabet, Symbol,
             -- Trans, transRule, TransRules, transSrc, transDst, transSymb)

import Data.Char (isAsciiLower)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, satisfy, newline, 
                    sepBy1, sepBy, many1, endBy,
                    digit, lower, char)

import Control.Monad ((<=<))
import Control.Arrow (left)


parseDFA :: String -> Either Error DFA
parseDFA = isDFA <=< left show . parse parserDFA ""
    where parserDFA = DFA <$> parseStates       <* newline
                          <*> parseAlphabet     <* newline
                          <*> parseState        <* newline 
                          <*> parseFinalStates  <* newline
                          <*> parseTransRules

-- | States are non-empty, finite set states separated by comma
parseStates :: Parser States
parseStates = parseState `sepBy1` sepComma

-- | State is a non-negative integer
parseState :: Parser State
parseState = read <$> many1 digit

-- | Alphabet is non-empty, finite set symbols, written in a string
parseAlphabet :: Parser Alphabet
parseAlphabet = many1 parseSymbol

-- | Symbol is a lower case character from a set [a-z]
parseSymbol :: Parser Symbol
parseSymbol = satisfy isAsciiLower

-- | Final states is finite set of states
parseFinalStates :: Parser States
parseFinalStates = parseState `sepBy` sepComma

-- | Optional finite set of transition rules
parseTransRules :: Parser TransRules
parseTransRules = parseTrans `endBy` newline

-- | Transition rule is a triple 'from State, using Symbol, to State'
-- | each separated by comma
parseTrans :: Parser Trans
parseTrans = transRule <$> parseState <* sepComma <*> parseSymbol <* sepComma
                       <*> parseState

-- | Comma character separator
sepComma :: Parser Char 
sepComma = char ','

-- TODO move to Types.hs???
-- | Syntax checks if is Deterministic Finite Automaton
-- |    initial state is in states
-- |    final states are in states
-- |    for each transition rule: 
-- |        each state is in states
-- |        each symbol is in alphabet
-- |    for each state:
-- |        has deterministic transition rules
isDFA :: DFA -> Either Error DFA
isDFA dfa@DFA{..} = if isFA && (not $ isNKA trans)
        then Right dfa
        else Left "Syntactically incorrect FA or non-deterministic"
    where 
        isFA = initial `elem` states
            && all inStates final
            && all (inStates . transSrc) trans
            && all (inAlphabet . transSymb) trans
            && all (inStates . transDst) trans
        inStates = (`elem` states)
        inAlphabet = (`elem` alphabet)

isTransAmbiguous :: Trans -> TransRules -> Bool
isTransAmbiguous _ [] = False
isTransAmbiguous t (x:ts) = isSameSrcSymb t x || isTransAmbiguous t ts
    where isSameSrcSymb = \(q, a, _) (p, c, _) -> q == p && a == c

-- | TODO
isNKA :: TransRules -> Bool
isNKA [] = False
isNKA (t:ts) = isTransAmbiguous t ts || isNKA ts



