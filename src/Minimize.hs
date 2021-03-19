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
import Data.List (sort, union, intersect, nub, nubBy, (\\), delete)


-- | TODO
minimizeDFA :: DFA -> DFA
minimizeDFA = rmSink . toReducedDFA . toFullyDefinedDFA . rmUnreachable 

-- | ----------------------------------------------------------------------
-- | 1. 
-- | Removes all unreachable states
-- TODO maybe Set fromList and later toList
rmUnreachable :: DFA -> DFA
rmUnreachable dfa@DFA{..} = 
        DFA reachableStates alphabet initial reachableFinal reachableTrans 
    where reachableStates = sort $ untilNoNext [initial] [] trans
          reachableTrans = [t | t@(q, _, _) <- trans, q `elem` reachableStates]
          reachableFinal = final `intersect` reachableStates

-- TODO to SET
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
-- |    Checks if the transition function is total, if not then the
-- |    DFA is extended by a SINK state
toFullyDefinedDFA :: DFA -> DFA
toFullyDefinedDFA dfa@DFA{..} = if null toSinkTrans
        then dfa
        else DFA (states ++ [sink]) alphabet initial final totalTrans 
    where sink = maximum states + 1
          toSinkTrans = [(q, a, sink) | q <- states, a <- alphabet, 
                                                    not $ existsTrans q a trans]
          totalTrans = sort $ trans ++ toSinkTrans ++ sinkTrans
          sinkTrans = [(sink, a, sink) | a <- alphabet]

-- | Whether a transition from a state using a symbol exists in transition rules
existsTrans :: State -> Symbol -> TransRules -> Bool
existsTrans _ _ [] = False
existsTrans q a ((p, c, _) : ts) = (q == p && a == c) || existsTrans q a ts

-- | ----------------------------------------------------------------------
-- | 3. Conversion of FULLY DEFINED DFA to REDUCED DFA
-- |    where no state is unreachable, and no two states are not distinguishable
-- | 
-- | TODO
toReducedDFA :: DFA -> DFA
toReducedDFA dfa@DFA{..} = if length states < 2 then dfa 
                           else DFA newStates alphabet newInit newFinal newTrans
    where statesInRel = indistToStates $ untilIndist [] (indist0Rel states final) dfa
          newStates   = [0..(length statesInRel -1)]
          newInit     = stateRelIdx initial statesInRel
          newFinal    = nub [eqF | f <- final, let eqF = stateRelIdx f statesInRel]
          newTrans    = nub [(eqP, a, eqQ) | (p, a, q) <- trans, 
                                let eqP = stateRelIdx p statesInRel, 
                                let eqQ = stateRelIdx (getDest p a trans) statesInRel]

-- | Indistinguishable relation for k = 0 is a list of pairs of states.
-- |    Is an equivalence relation on states.
indist0Rel :: States -> States -> [(State, State)]
indist0Rel states final = concat $ map (relEquiv) [final, states \\ final]
    where relEquiv st = [(p, q) | p <- st, q <- st]

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

-- | ----------------------------------------------------------------------
-- | 4. Remove possible SINK state
-- |    a non-final state with transition rules only to itself
rmSink :: DFA -> DFA
rmSink dfa@DFA{..} = if notExistsSink then dfa
                                      else DFA newStates alphabet initial final newTrans
    where (sinkState, sinkRules) = findSink (states \\ final) (length alphabet) trans
          notExistsSink = null $ sinkRules
          newStates     = delete sinkState states
          newTrans      = [t | t@(_, _, q) <- trans, q /= sinkState] \\ sinkRules

-- | Checks if a list of transition rules with the same "src" and "dest" state
-- |    is defined for each symbol of the alphabet (has the same length
-- |    as the alphabet)
findSink :: States -> Int -> TransRules -> (State, TransRules)
findSink []     _     _  = (-1, [])
findSink (s:st) alLen tr = if length sinkRules == alLen then (s, sinkRules)
                                                        else findSink st alLen tr
    where sinkRules = [t | t@(p, _, q) <- tr, p == q && p == s]


