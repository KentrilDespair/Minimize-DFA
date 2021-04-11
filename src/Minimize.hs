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
              TransFnc, toTransFnc, toTransFncL, getDest, die)

-- import System.Exit (die) TODO Merlin compatibility 

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
        DFA reachableStates alpha initial reachableFinal reachableTrans 
    where reachableStates = untilNoNext Set.empty (Set.singleton initial) trans
          reachableTrans  = Set.filter (\(q,_,_) -> q `Set.member` reachableStates) trans
          reachableFinal  = final `Set.intersection` reachableStates

-- | Finds all states we can get into from the initial state
untilNoNext :: States -> States -> TransRules -> States
untilNoNext s_0 s_1 ts
    | s_0 == s_1 = s_1
    | otherwise  = untilNoNext s_1 (s_1 `Set.union` possibleDests) ts
    where possibleDests = stepThrough s_1 ts

-- | For all input states finds all states they can get into using any symbol
stepThrough :: States -> TransRules -> States
stepThrough st ts = eachStateDests $ unions transFromStates
    where fromState s     = Set.filter (\(q,_,_) -> q == s) ts
          transFromStates = Set.map fromState st
          eachStateDests  = Set.map transDst
          unions          = Set.foldl Set.union Set.empty  -- TODO Merlin compatibility

-- | ----------------------------------------------------------------------
-- | 2.
-- | Converts to equivalent fully defined DFA
-- |    Checks if the transition function (rules) is total, if not then 
-- |    the DFA is extended by a SINK state
toFullyDefinedDFA :: DFA -> DFA
toFullyDefinedDFA dfa@DFA{..} = if null toSinkTrans
        then dfa
        else DFA (sink `Set.insert` states) alpha initial final totalTrans 
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
toReducedDFA dfa@DFA{..} = if Set.size states < 2
                           then oneStateDFA dfa newInit
                           else DFA (Set.fromList newStates) 
                                    alpha 
                                    newInit 
                                    newFinal
                                    (Set.fromList newTrans)
    where newStates   = [0..totalEqCls]
          newInit     = 0
          newFinal    = Set.map getEq final
          newTrans    = [(eq,a,eqP) | eq <- newStates, a <- al, 
                                      let dst = getDest (getSt eq,a) transFnc,
                                      let eqP = getEq dst]
          totalEqCls  = length statesInRel -1
          statesInRel = statesInRelation indistRel st
          indistRel   = untilIndist [] (indist0Rel st fs) al transFnc
    -- Defined mappings between states and equiv. classes
          (st2eq, eq2st) = relabelInOrder initial ts statesInRel
          getEq s     = st2eq Map.! s
          getSt e     = eq2st Map.! e
    -- List variants
          st          = stateList dfa
          al          = alphaList dfa
          fs          = finalList dfa
          ts          = transList dfa
          transFnc    = toTransFncL ts

--------------------------------------------------------------------------------
-- | Returns 'Reduced DFA' that has only ONE state - the initial state
oneStateDFA :: DFA -> State -> DFA
oneStateDFA dfa@DFA{..} init = DFA (Set.singleton init) 
                                   alpha
                                   init
                                   oneFinal
                                   (Set.fromList [(init,a,init) | a <- al])
    where oneFinal = if Set.null final then final 
                                       else Set.singleton init
          al = alphaList dfa

-- | Converts the indistinguishable relation ([(p,q)]) to a list of lists of 
-- |    states in relation, e.g. [[1,6],[2,5],[3,4]]
statesInRelation :: [StatePair] -> [State] -> [[State]]
statesInRelation indist st = nub [map snd $ filter inRelation indist | q <- st, 
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
-- | Constructs mappings from the ordered equivalence classes "eqClsOrdered"
-- | Returns mapping: State to Equivalence class label
-- |                  Equivalence class label to first State in the class
relabelInOrder :: State -> [Trans] -> [[State]] -> (Map.Map State State, 
                                                    Map.Map State State)
relabelInOrder init ts stInRel = (m, mapEq2States m)
    where m = foldl (flip mapStates2Eq) Map.empty eqClsOrdered
          eqClsOrdered = toEqInorder 0 [findIndist init stInRel] ts stInRel

-- | Relabels the equivalence classes (each list in "relst") in the order
-- |    how the states are visited from the intiial state "init" using a
-- |    list of transitions TODO transFnc TODO
-- | Makes sure that the order of the classes is preserved, only unique
-- |    classes are present, and ends when all classes were visited 
-- |    (don't have to visit all states).
toEqInorder :: Int -> [[State]] -> [Trans] -> [[State]] -> [[State]]
toEqInorder i indist ts relst
    | i >= length indist = indist
    | otherwise          = toEqInorder (i+1) (indist ++ newIndist) ts relst
    where newIndist = nub $ map (`findIndist` relst) newSt
          newSt     = filter isUniq (getDests curState ts)
          -- States not already in indist
          isUniq s  = not $ inAnyEq s
          inAnyEq s = any (s `elem`) indist
          curState  = head $ indist !! i

-- | Returns destination states of state "s" in list of transition rules
-- |    Must be unique and the order must be preserved as visited (NOT sorted!)
-- TODO use map??
getDests :: State -> [Trans] -> [State]
getDests s ts = nub $ map transDst $ filter (\(q,_,_) -> q == s) ts

-- | Returns all the indistinguishable states from this state
-- |    in the relation of indistinguishable states
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
                                      else DFA newStates alpha initial final newTrans
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
