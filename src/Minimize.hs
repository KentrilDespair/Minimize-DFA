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
-- TODO qualified only those that are needed
import qualified Data.Set as Set (
    empty, singleton, fromList, toList, null, size, map, findMax, filter, insert,
    member, union, unions, intersection, (\\), delete)
import qualified Data.Map as Map (Map, empty, insert, (!), null, elems, fromList, assocs)


-- | TODO
-- minimizeDFA = rmSink . toReducedDFA . toFullyDefinedDFA . rmUnreachable 
minimizeDFA :: DFA -> DFA
minimizeDFA = toReducedDFA . toFullyDefinedDFA . rmUnreachable 

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
-- | TODO rename
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
-- | TODO
-- TODO remove symmetry in StatePair
-- TODO maybe to Set for further use??
toReducedDFA :: DFA -> DFA
toReducedDFA dfa@DFA{..} = if Set.size states < 2 then dfa 
                           else DFA newStates alpha newInit newFinal newTrans
    where newStates   = Set.fromList [0..totalEqCls]
          newInit     = 0
          newFinal    = Set.map (\f -> getEq f) final
          newTrans    = Set.fromList [(eq,a,eqP) | eq <- (Set.toList newStates), a <- alphaList dfa, 
                                         let eqP = getEq (getDest (getSt eq) a (transList dfa))]

          statesInRel = statesInRelation (untilIndist [] [] dfa) (statesList dfa)
          totalEqCls  = length statesInRel -1
    -- Defined mappings between states and equiv. classes
          (st2eq, eq2st) = relabelInOrder init (transList dfa) statesInRel
          getEq s     = st2eq Map.! s
          getSt e     = eq2st Map.! e

--------------------------------------------------------------------------------
-- | TODO
untilIndist :: [StatePair] -> [StatePair] -> DFA -> [StatePair]
untilIndist [] [] dfa@DFA{..} = untilIndist [] indistRel0 dfa
    where indistRel0  = concat $ map (relEquiv) [finalList dfa, Set.toList $ states Set.\\ final]
          relEquiv st = [(p, q) | p <- st, q <- st]
untilIndist indist0 indist1 dfa@DFA{..}
    | indist0 /= indist1 = untilIndist indist1 indistK dfa
    | otherwise          = indist1
    where indistK     = nextIndist indist1 al ts
          al          = alphaList dfa
          ts          = transList dfa
        
indist0Rel :: States -> States -> [StatePair]
indist0Rel states final = concat $ map (relEquiv) 
                            [Set.toList final, Set.toList $ states Set.\\ final]
    where relEquiv st = [(p, q) | p <- st, q <- st]

-- | Checks the property of the next indistinguishable relation
nextIndist :: [StatePair] -> [Symbol] -> [Trans] -> [StatePair] 
nextIndist indistK al ts = [(p, q) | (p, q) <- indistK, allDestsIndist (destPairs p q al ts)]
    where allDestsIndist = all (`elem` indistK) 

destPairs :: State -> State -> [Symbol] -> [Trans] -> [StatePair]
destPairs p q al ts = [(d1, d2) | a <- al, let d1 = getDest p a ts,
                                           let d2 = getDest q a ts]

-- | Returns the destination state we get into from state using symbol
-- | expects FULLY DEFINED DFA's transition rules
-- -- TODO error or use find
-- TODO one function in types??
getDest :: State -> Symbol -> [Trans] -> State
getDest p a ((q, c, d):ts)
    | p == q && a == c = d
    | null ts          = error "getDest SHOULD NOT HAPPEN"
    | otherwise        = getDest p a ts

-- | Returns a list of lists of indistinguishable states 
-- |    e.g. {[1,6],[2,5],[3,4]}
-- TODO better, return set
statesInRelation :: [StatePair] -> [State] -> [[State]]
statesInRelation sp st = nub [map snd $ filter inRelation sp | q <- st, 
                                        let inRelation = \(p,_) -> p == q]
--------------------------------------------------------------------------------
-- | init transList Possible set of those states in rel
relabelInOrder :: State -> [Trans] -> [[State]] -> (Map.Map State State, Map.Map State State)
relabelInOrder init ts stInRel = (m, mapEq2States m)
    where eqClsOrdered = toEqInorder 0 [findIndist init stInRel] ts stInRel
          m = foldl (\macc st -> mapStates2Eq st macc) (Map.empty) eqClsOrdered

-- | Relables transition rules using equivalence classes that each state is in
-- |    starting from initial state
-- toDestInorder init [init] {[1,6], [2,5], [3,4]}
-- [1,6,2, 1,5, 5,4, ], LOOP
-- [1,2,4] not already added a neni ve stejne skupine
--            1 [1] [findIndist 1 stRels]
toEqInorder :: Int -> [[State]] -> [Trans] -> [[State]] -> [[State]]
toEqInorder i indist ts relst
    | i >= length indist = indist
    -- | null newSt         = indist
    | otherwise          = toEqInorder (i+1) (indist ++ newIndist) ts relst
    where s = head $ indist !! i
          newSt = filter isUniq (getDests s ts)
          isUniq = \s -> not $ any (s `elem`) indist
          newIndist = map (\s -> findIndist s relst) newSt

-- Not ordered! BUT UNIQUE
getDests :: State -> [Trans] -> [State]
getDests s ts = nub $ map transDst $ filter (\(q,_,_) -> q == s) ts

-- | Returns the indistinguishable states from the state
findIndist :: State -> [[State]] -> [State]
findIndist s (st:stRels)
    | s `elem` st = st
    | null stRels = error "State should exist"
    | otherwise   = findIndist s stRels

-- | Maps states to the same equivalence class, always creates a new equivalence
-- |    class which is +1 the previous
mapStates2Eq :: [State] -> Map.Map State State -> Map.Map State State
mapStates2Eq st m = foldl (\macc s -> Map.insert s newEq macc) m st
    where newEq = if Map.null m then 0 else maxEq +1
          maxEq = maximum $ Map.elems m


-- | Using Map State -> Eq, returns a Map Eq -> State
-- |    Because the states under one eq. class are indistinguishable only 
-- |    the first is used.
mapEq2States :: Map.Map State State -> Map.Map State State
mapEq2States = Map.fromList . map flipPair . nubBy firstInEq . Map.assocs
    where firstInEq = \(p,q) (u,v) -> q == v
          flipPair  = \(p,q) -> (q,p)


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


