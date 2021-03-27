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

-- For UnitTest.hs nothing is explicitly exported
module Minimize where

import Types (State, States, StatePair, Symbol, Trans, TransRules, transDst,
              stateList, alphaList, finalList, transList, DFA(..), 
              TransFnc, toTransFnc, toTransFncL, getDest)

import System.Exit (die)

import Data.List (nub, nubBy, (\\))
import qualified Data.Set as Set
import qualified Data.Map as Map


-- | Returns a reduced DFA with total transition function
-- |    *SINK state is not removed*
minimizeDFA :: DFA -> DFA
minimizeDFA = {- rmSink . -} toReducedDFA . toFullyDefinedDFA . rmUnreachable 

-- | ----------------------------------------------------------------------
-- | 1. 
-- | Elimination of unreachable states
rmUnreachable :: DFA -> DFA
rmUnreachable dfa@DFA{..} = 
        DFA reachableStates alpha init reachableFinal reachableTrans 
    where reachableStates = untilNoNext Set.empty (Set.singleton init) trans
          reachableTrans  = Set.filter (\(q,_,_) -> q `Set.member` reachableStates) trans
          reachableFinal  = final `Set.intersection` reachableStates

-- | Finds all states we can get into from initial states
untilNoNext :: States -> States -> TransRules -> States
untilNoNext s_0 s_1 ts
    | s_0 == s_1 = s_1
    | otherwise  = untilNoNext s_1 (s_1 `Set.union` possibleDests) ts
    where possibleDests = stepThrough s_1 ts

-- | For all input states finds all states they can get into using any symbol
stepThrough :: States -> TransRules -> States
stepThrough st ts = eachStateDests $ Set.unions transFromStates
    where fromState s     = Set.filter (\(q,_,_) -> q == s) ts
          transFromStates = Set.map fromState st
          eachStateDests  = Set.map transDst

-- | ----------------------------------------------------------------------
-- | 2.
-- | Converts to equivalent fully defined DFA
-- |    Checks if the transition function (rules) is total, if not then 
-- |    the DFA is extended by a SINK state
toFullyDefinedDFA :: DFA -> DFA
toFullyDefinedDFA dfa@DFA{..} = if null toSinkTrans
        then dfa
        else DFA (sink `Set.insert` states) alpha init final totalTrans 
    where sink        = Set.findMax states + 1
          -- TODO test
          --totalTrans  = trans `Set.union` Set.fromList (toSinkTrans ++ sinkTrans)
          totalTrans  = Set.fromList (ts ++ toSinkTrans ++ sinkTrans)
          toSinkTrans = [(q,a,sink) | q <- st, a <- al, notExistTrans (q,a) transFnc]
          sinkTrans   = [(sink,a,sink) | a <- al]
          st          = stateList dfa
          al          = alphaList dfa
          ts          = transList dfa
          transFnc    = toTransFnc trans

-- | Whether there are NO transition rules from a state using a symbol
notExistTrans :: (State, Symbol) -> TransFnc -> Bool
notExistTrans k m = k `Map.notMember` m

-- | ----------------------------------------------------------------------
-- | 3. Conversion of FULLY DEFINED DFA to REDUCED DFA
-- |    where no state is unreachable, and no two states are indistinguishable
-- | 
toReducedDFA :: DFA -> DFA
toReducedDFA dfa@DFA{..} = if Set.size states < 2 then dfa 
                           else DFA (Set.fromList newStates) alpha newInit 
                                    newFinal (Set.fromList newTrans)
    where newStates   = [0..totalEqCls]
          newInit     = 0
          newFinal    = Set.map getEq final
          newTrans    = [(eq,a,eqP) | eq <- newStates, a <- al, 
                                      let dst = getDest ((getSt eq),a) transFnc,
                                      let eqP = getEq dst]
          totalEqCls  = length statesInRel -1
          statesInRel = statesInRelation st indistRel
          indistRel   = untilIndist [] (indist0Rel st fs) al transFnc
    -- Defined mappings between states and equiv. classes
          (st2eq, eq2st) = relabelInOrder init ts statesInRel
          getEq s     = st2eq Map.! s
          getSt e     = eq2st Map.! e
    -- List variants
          st          = stateList dfa
          al          = alphaList dfa
          fs          = finalList dfa
          ts          = transList dfa
          transFnc    = toTransFncL ts

--------------------------------------------------------------------------------
-- | Returns a list of lists of indistinguishable states 
-- |    e.g. {[1,6],[2,5],[3,4]}
-- TODO better, return set vs nub
-- TODO input what are those
statesInRelation :: [State] -> [StatePair] -> [[State]]
statesInRelation st indist = nub [map snd $ filter inRelation indist | q <- st, 
                                        let inRelation = \(p,_) -> p == q]

-- | Indistinguishable relation for 'k = 0', on pairs of states (relation of equivalence)
indist0Rel :: [State] -> [State] -> [StatePair]
indist0Rel st fs = relEquiv fs ++ relEquiv (st \\ fs)
    where relEquiv qs = [(p, q) | p <- qs, q <- qs]

-- | Until each state is indistinguishable for each value of 'k'
untilIndist :: [StatePair] -> [StatePair] -> [Symbol] -> TransFnc -> [StatePair]
untilIndist indist0 indist1 al m
    | indist0 /= indist1 = untilIndist indist1 indistK al m
    | otherwise          = indist1
    where indistK = nextIndist indist1 al m
        
-- | Checks the property of the next indistinguishable relation
-- |    all destination states of states in relation are also in relation
nextIndist :: [StatePair] -> [Symbol] -> TransFnc -> [StatePair] 
nextIndist indistK al m = [(p, q) | (p, q) <- indistK, destsIndist (destPairs p q al m)]
    where destsIndist = all (`elem` indistK) 

-- | Returns pairs of destination states of two states for each symbol
destPairs :: State -> State -> [Symbol] -> TransFnc -> [StatePair]
destPairs p q al m = [(d1, d2) | a <- al, let d1 = getDest (p,a) m,
                                          let d2 = getDest (q,a) m]

--------------------------------------------------------------------------------
-- | 
relabelInOrder :: State -> [Trans] -> [[State]] -> (Map.Map State State, Map.Map State State)
relabelInOrder init ts stInRel = (m, mapEq2States m)
    where m = foldl (flip mapStates2Eq) Map.empty eqClsOrdered
          eqClsOrdered = toEqInorder 0 [findIndist init stInRel] ts stInRel

-- | Relables transition rules using equivalence classes that each state is in
-- |    starting from initial state
-- toDestInorder init [init] {[1,6], [2,5], [3,4]}
-- [1,6,2, 1,5, 5,4, ], LOOP
-- [1,2,4] not already added a neni ve stejne skupine
-- What are ethe params?
-- How does it work?
toEqInorder :: Int -> [[State]] -> [Trans] -> [[State]] -> [[State]]
toEqInorder i indist ts relst
    | i >= length indist = indist
    | otherwise          = toEqInorder (i+1) (indist ++ newIndist) ts relst
    where newIndist = map (`findIndist` relst) newSt
          newSt     = filter isUniq (getDests curState ts)
          isUniq s  = not $ any (s `elem`) indist
          curState  = head $ indist !! i

-- Not ordered! BUT UNIQUE
getDests :: State -> [Trans] -> [State]
getDests s ts = nub $ map transDst $ filter (\(q,_,_) -> q == s) ts

-- | Returns the indistinguishable states from the state
findIndist :: State -> [[State]] -> [State]
findIndist _ [] =  error "Each state should exist"
findIndist s (st:stRels) = if s `elem` st then st 
                                          else findIndist s stRels

-- | Maps states to the same equivalence class, always creates a new equivalence
-- |    class which is +1 the previous
mapStates2Eq :: [State] -> Map.Map State State -> Map.Map State State
mapStates2Eq st m = foldl (\macc s -> Map.insert s newEq macc) m st
    where newEq = if Map.null m then 0 else maxEq +1
          maxEq = maximum $ Map.elems m

-- | Using Map: State to EqClass, returns a Map: EqClass to State
-- |    Because the states under one eq. class are indistinguishable, only 
-- |    the first is used as the value under the eq. class key.
mapEq2States :: Map.Map State State -> Map.Map State State
mapEq2States = Map.fromList . map flipPair . nubBy firstInEq . Map.assocs
    where flipPair  (p,q)       = (q,p)
          firstInEq (_,q) (_,v) = q == v
          
-- | ----------------------------------------------------------------------
-- | 4. 
-- | Remove possible SINK state
-- |    - a non-final state with transition rules only to itself
rmSink :: DFA -> DFA
rmSink dfa@DFA{..} = if notExistsSink then dfa
                                      else DFA newStates alpha init final newTrans
    where (sinkState, sinkRules) = findSink (st \\ fs) (Set.size alpha) trans
          notExistsSink = Set.null $ sinkRules
          newStates     = Set.delete sinkState states
          newTrans      = Set.filter (\(_,_,q) -> q /= sinkState) trans Set.\\ sinkRules
          st            = stateList dfa
          fs            = finalList dfa
          -- TODO test
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


