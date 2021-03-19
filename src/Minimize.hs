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
-- TODO clear from unused
import Data.List (nub, nubBy, (\\), delete, sort)
import qualified Data.Set as Set (
    empty, singleton, fromList, toList, null, size, map, findMax, filter, insert,
    member, union, unions, intersection, (\\), delete)


-- | TODO
minimizeDFA :: DFA -> DFA
minimizeDFA = rmSink . toReducedDFA . toFullyDefinedDFA . rmUnreachable 

-- | ----------------------------------------------------------------------
-- | 1. 
-- | Removes all unreachable states
rmUnreachable :: DFA -> DFA
rmUnreachable dfa@DFA{..} = 
        DFA reachableStates alpha init reachableFinal reachableTrans 
    where reachableStates = untilNoNext Set.empty (Set.singleton init) trans
          reachableTrans  = Set.filter (\(q, _, _) -> q `Set.member` reachableStates) trans
          reachableFinal  = final `Set.intersection` reachableStates

-- | Returns all states we can get into from the initial state
untilNoNext :: States -> States -> TransRules -> States
untilNoNext s_0 s_1 ts
    | s_0 == s_1 = s_1
    | otherwise  = untilNoNext s_1 (s_1 `Set.union` possibleDests) ts
    where possibleDests = stepThrough s_1 ts

-- | For all states finds all states they can get into using any symbol
stepThrough :: States -> TransRules -> States
stepThrough st ts = Set.map transDst $ Set.unions $ Set.map transSameSrc st
    where transSameSrc s = Set.filter (\(q, _, p) -> q == s) ts

-- | ----------------------------------------------------------------------
-- | 2.
-- | Converts to equivalent fully defined DFA
-- |    Checks if the transition function (rules) is (are) total, if not then 
-- |    the DFA is extended by a SINK state
toFullyDefinedDFA :: DFA -> DFA
toFullyDefinedDFA dfa@DFA{..} = if null toSinkTrans
        then dfa
        else DFA (Set.insert sink states) alpha init final totalTrans 
    where sink        = Set.findMax states + 1
          totalTrans  = trans `Set.union` (Set.fromList $ toSinkTrans ++ sinkTrans)
          toSinkTrans = [(q, a, sink) | q <- st, a <- al, notExistTrans q a trans]
          sinkTrans   = [(sink, a, sink) | a <- al]
          st          = statesList dfa
          al          = alphaList dfa

-- TODO same as find Dest
-- | Whether transition rules from a state using a symbol not exist
notExistTrans :: State -> Symbol -> TransRules -> Bool
notExistTrans q a ts = Set.null $ Set.filter isSameSrcSymb ts
    where isSameSrcSymb = \(p, c, _) -> p == q && c == a

-- | ----------------------------------------------------------------------
-- | 3. Conversion of FULLY DEFINED DFA to REDUCED DFA
-- |    where no state is unreachable, and no two states are indistinguishable
-- | 
-- | TODO
-- | TODO PREZNACOVANI PODLE PRAVIDEL !!!
toReducedDFA :: DFA -> DFA
toReducedDFA dfa@DFA{..} = if Set.size states < 2 then dfa 
                           else DFA newStates alpha newInit newFinal newTrans
    where statesInRel = sort $ indistToStates $ untilIndist [] [] dfa
          newStates   = Set.fromList [0..(length statesInRel -1)]
          newInit     = stateRelIdx init statesInRel
          newFinal    = Set.map (\f -> stateRelIdx f statesInRel) final
          -- newFinal    = nub [eqF | f <- final, let eqF = stateRelIdx f statesInRel]
          newTrans    = Set.map (\(p, a, q) -> (stateRelIdx p statesInRel, a, 
                                                     stateRelIdx (getDest p a (transList dfa)) statesInRel))
                                     trans
          --newTrans    = nub [(eqP, a, eqQ) | (p, a, q) <- trans, 
          --                      let eqP = stateRelIdx p statesInRel, 
          --                      let eqQ = stateRelIdx (getDest p a trans) statesInRel]


untilIndist :: [StatePair] -> [StatePair] -> DFA -> [StatePair]
untilIndist indist0 indist1 dfa@DFA{..}
    | null indist1       = untilIndist indist0 indistRel0 dfa
    | indist0 /= indist1 = untilIndist indist1 indistK dfa
    | otherwise          = indist1
    where indistRel0  = concat $ map (relEquiv) [finalList dfa, Set.toList $ states Set.\\ final]
          indistK     = nextIndist indist1 al ts
          relEquiv st = [(p, q) | p <- st, q <- st]
          al          = alphaList dfa
          ts          = transList dfa
        
nextIndist :: [StatePair] -> [Symbol] -> [Trans] -> [StatePair] 
nextIndist indistK al ts = [(p, q) | (p, q) <- indistK, allDestsIndist (destPairs p q al ts)]
    where allDestsIndist = all (`elem` indistK) 

destPairs :: State -> State -> [Symbol] -> [Trans] -> [StatePair]
destPairs p q al ts = [(d1, d2) | a <- al, let d1 = getDest p a ts,
                                           let d2 = getDest q a ts]

-- | Returns the destination state we get into from state using symbol
-- | expects FULLY DEFINED DFA's transition rules
-- -- TODO error or use find
getDest :: State -> Symbol -> [Trans] -> State
getDest p a ((q, c, d) : ts)
    | p == q && a == c = d
    | null ts          = error "getDest SHOULD NOT HAPPEN"
    | otherwise        = getDest p a ts

-- | Converts the pairs of indistinguishable states in the relation of equivalence
-- |    to a list of list of states that correspond to each new state
indistToStates :: [StatePair] -> [States]
indistToStates ((p, _) : indist) = equivToStates p (Set.singleton p) (rmSymmetry indist)
    where rmSymmetry = nubBy (\(p, q) (u, v) -> p == v && q == u)

-- | 
equivToStates :: State -> States -> [StatePair] -> [States]
equivToStates _ st [] = st : []
equivToStates s st ((p, q) : indist)
    | s == p    = if s /= q then equivToStates s (Set.insert q st) (rmReflexivity indist)
                            else error "EQUIV TO STATES SHOULD NOT HAPPEN"
    | p == q    = st : equivToStates p (Set.singleton p) indist
    | otherwise = error "EQUIV TO STATES SHOULD NOT HAPPEN"
    where rmReflexivity = delete (q, q)

-- | Returns the index of the state in the 'states in relation list'
-- |    where n-th list that has that state is the index (the equivalence class)
stateRelIdx :: State -> [States] -> Int
stateRelIdx s (st : sst) = if s `Set.member` st then 0 
                                                else 1 + stateRelIdx s sst

-- | ----------------------------------------------------------------------
-- | 4. Remove possible SINK state
-- |    a non-final state with transition rules only to itself
rmSink :: DFA -> DFA
rmSink dfa@DFA{..} = if notExistsSink then dfa
                                      else DFA newStates alpha init final newTrans
    where (sinkState, sinkRules) = findSink (Set.toList $ states Set.\\ final) (Set.size alpha) trans
          notExistsSink = Set.null $ sinkRules
          newStates     = Set.delete sinkState states
          newTrans      = (Set.filter (\(_, _, q) -> q /= sinkState) trans) Set.\\ sinkRules
          --newTrans      = [t | t@(_, _, q) <- trans, q /= sinkState] \\ sinkRules

-- | Checks if a list of transition rules with the same "src" and "dest" state
-- |    is defined for each symbol of the alphabet (has the same length
-- |    as the alphabet)
findSink :: [State] -> Int -> TransRules -> (State, TransRules)
findSink []     _     _  = (-1, Set.empty)
findSink (s:st) alLen tr = if Set.size sinkRules == alLen then (s, sinkRules)
                                                          else findSink st alLen tr
    where sinkRules = Set.filter (\(p, _, q) -> p == q && p == s) tr
    -- where sinkRules = [t | t@(p, _, q) <- tr, p == q && p == s]


