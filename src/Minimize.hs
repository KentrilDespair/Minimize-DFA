{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Module consisting of functions for DFA minimization.
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards #-}

module Minimize where

-- TODO what is imported, rather describe in Types
import Types

import System.Exit (die)
import Data.List (union, intersect, nub, nubBy, (\\), delete)


-- | TODO
minimizeDFA :: DFA -> DFA
minimizeDFA = toReducedDFA . toFullyDefinedDFA . rmUnreachable 

-- | ----------------------------------------------------------------------
-- | 1. 
-- | Removes all unreachable states
rmUnreachable :: DFA -> DFA
rmUnreachable dfa@DFA{..} = 
        DFA reachableStates alphabet initial reachableFinal reachableTrans 
    where reachableStates = untilNoNext [initial] [] trans
          reachableTrans = [t | t@(q, _, _) <- trans, q `elem` reachableStates]
          reachableFinal = final `intersect` reachableStates

untilNoNext :: States -> States -> TransRules -> States
untilNoNext s_0 s_1 ts
    | null $ s_0 \\ s_1 = s_1
    | otherwise         = untilNoNext (s_0 `union` uniqueDsts) s_0 ts
    where uniqueDsts = nub $ stepThrough s_0 ts

-- | For all states finds all states they can get into using any symbol
stepThrough :: States -> TransRules -> States
stepThrough st ts = concat [dsts | s <- st, let dsts = findDsts s ts]

-- | Returns all destination states we can get into from a state
-- |    using any symbol
findDsts :: State -> TransRules -> States
findDsts s ts = [p | (q, _, p) <- ts, q == s]


-- | ----------------------------------------------------------------------
-- | 2.
-- | Converts to equivalent fully defined DFA
-- |    Checks if the transition function is total
-- |    if not then the SINK state is added
toFullyDefinedDFA :: DFA -> DFA
toFullyDefinedDFA dfa@DFA{..} = if null newTransRules 
        then dfa
        else DFA (states ++ [sinkState]) alphabet initial final newTransRules
    where sinkState = maximum states + 1
          newTransRules = trans ++ (transToSink sinkState states alphabet trans) ++ sinkTrans
          sinkTrans = [(sinkState, a, sinkState) | a <- alphabet]


-- | Sink state, States, Alphabet, Transition rules
transToSink :: State -> States -> Alphabet -> TransRules -> TransRules
transToSink sink st ep ts = [tSink | s <- st, a <- ep, 
                                     let tSink = (s, a, sink), not $ tSink `existsTrans` ts]

-- | Whether a transition from a state using a symbol exists in transition rules
existsTrans :: Trans -> TransRules -> Bool
existsTrans _ [] = False
existsTrans t (x:ts) = if isSameSrcSymb t x then True
                                            else existsTrans t ts
    where isSameSrcSymb = \(q, a, _) (p, c, _) -> q == p && a == c

-- | ----------------------------------------------------------------------
-- | 3. Conversion of FULLY DEFINED DFA to REDUCED DFA
-- |    where no state is unreachable, and no two states are not distinguishable
-- | 
-- | TODO
toReducedDFA :: DFA -> DFA
toReducedDFA dfa@DFA{..} = if length states < 2 
        then dfa 
        else DFA newStates alphabet newInit newFinal newTrans
    where indist0     = concat $ map (relEquiv) [final, states \\ final]
          relEquiv st = [(p, q) | p <- st, q <- st]
          statesInRel = indistToStates $ untilIndist [] indist0 dfa
          newStates   = concat statesInRel -- [0..(length statesInRel -1)]
          newInit     = stateRelIdx initial statesInRel
          newFinal    = [eqF | f <- final, let eqF = stateRelIdx f statesInRel]
          newTrans    = nub [(eqP, a, eqQ) | (p, a, q) <- trans, 
                                let eqP = stateRelIdx p statesInRel, 
                                let eqQ = stateRelIdx (getDest p a trans) statesInRel]

-- Until all pairs of states are indistinguishable
untilIndist :: [(State, State)] -> [(State, State)] -> DFA -> [(State, State)]
untilIndist indist0 indist1 dfa@DFA{..} = if indist0 /= indist1 
            then untilIndist indist1 indistK dfa
            else indist1
    where indistK = nextIndist indist1 alphabet trans  

nextIndist :: [(State, State)] -> Alphabet -> TransRules -> [(State, State)] 
nextIndist indistK alpha trans = [(p, q) | (p, q) <- indistK, allDestsIndist (destPairs p q alpha trans)]
    where allDestsIndist = all (`elem` indistK) 

destPairs :: State -> State -> Alphabet -> TransRules -> [(State, State)]
destPairs p q al ts = [(d1, d2) | a <- al, let d1 = getDest p a ts,
                                           let d2 = getDest q a ts]

-- | Returns the destination state we get into from state using symbol
-- | expects FULLY DEFINED DFA's transition rules
getDest :: State -> Symbol -> TransRules -> State
getDest p a ((q, c, d) : ts) = if p == q && a == c then d
                                                   else getDest p a ts

-- | Converts the pairs of indistinguishable states in the relation of equivalence
-- |    to a list of list of states that correspond to each new state
indistToStates :: [(State, State)] -> [States]
indistToStates ((p, _) : indist) = equivToStates p [p] (rmSymmetry indist)
    where rmSymmetry = nubBy (\(p, q) (u, v) -> p == v && q == u)

-- | 
equivToStates :: State -> States -> [(State, State)] -> [States]
equivToStates _ st [] = st : []
equivToStates s st ((p, q) : indist)
    | s == p    = if s /= q then equivToStates s (q : st) (rmReflexivity indist)
                            else error "EQUIV TO STATES SHOULD NOT HAPPEN"
    | p == q    = st : equivToStates p [p] indist
    | otherwise = error "EQUIV TO STATES SHOULD NOT HAPPEN"
    where rmReflexivity = delete (q, q)

-- | Returns the index of the state in the 'states in relation list'
-- |    where n-th list that has that state is the index (the equivalence class)
stateRelIdx :: State -> [States] -> Int
stateRelIdx s (st : sst) = if s `elem` st then 0 
                                          else 1 + stateRelIdx s sst


