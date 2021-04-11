{------------------------------------------------------------------------------
 - Project: DKA-2-MKA
 -          (Deterministic Finite Automata to Minimal Finite Automata)
 -          Functional and Logical Programming 2020 / 2021
 - Author : Martin Smutny, xsmutn13
 - Date   : 03.03.2021
 -
 - Module for testing various functions from Minimize module.
 -  Is not part of the main program.
 ------------------------------------------------------------------------------}

{-# LANGUAGE RecordWildCards #-}

import Types
    ( DFA(..),
      Trans,
      Symbol,
      StatePair,
      State,
      stateList,
      alphaList,
      finalList,
      transList,
      TransFnc,
      toTransFnc )
import Minimize
    ( rmUnreachable,
      toFullyDefinedDFA,
      toReducedDFA,
      indist0Rel,
      nextIndist,
      statesInRelation,
      minimizeDFA )

import qualified Data.Set as Set
import Data.List (sort)

-- =============================================================================
-- | Setup of the data to be tested and results of the tests

-- | All test relevant data for a certain DFA 
-- |    'dfa':       Original, input DFA
-- |    'reachable': DFA after removal of unreachable states
-- |    'fdefined':  Fully defined DFA
-- |    'indistRel': List of indistinguishable relations (step by step)
-- |    'relStates': List of list of states that are in relation
-- |    'reduced':   Reduced DFA
data DFASuite = DFASuite { dfa       :: DFA 
                         , reachable :: DFA
                         , fdefined  :: DFA
                         , indistRel :: [[StatePair]]
                         , relStates :: [[State]]
                         , reduced   :: DFA
                         }

toSetDFA :: [State] -> [Symbol] -> State -> [State] -> [Trans] -> DFA
toSetDFA s a i f t = DFA ss as i fs ts
    where ss = Set.fromList s
          as = Set.fromList a
          fs = Set.fromList f
          ts = Set.fromList t

--------------------------------------------------------------------------------
-- | Reference DFA
dfa0 :: DFA
dfa0 = toSetDFA states alpha init final trans 
    where states = [1,2,3,4,5,6]
          alpha  = "ab"
          init   = 1
          final  = [1,6]
          trans  = [(1,'a',6),(1,'b',2),(2,'a',5),(2,'b',4),(3,'a',3),(3,'b',6),
                   (4,'a',4),(4,'b',1),(5,'a',2),(5,'b',3),(6,'a',1),(6,'b',5)]

dfa0_suite :: DFASuite
dfa0_suite = DFASuite org reach fdef indist relst reduc
    where org = dfa0
          reach = dfa0
          fdef = dfa0
          indist = [[(1,1),(1,6),(6,1),(6,6), (2,2),(2,3),(2,4),(2,5), 
                     (3,2),(3,3),(3,4),(3,5), (4,2),(4,3),(4,4),(4,5),
                     (5,2),(5,3),(5,4),(5,5)],
                    [(1,1),(1,6),(6,1),(6,6), (2,2),(2,5), (3,3),(3,4), (4,3),(4,4),
                     (5,2),(5,5)],
                    [(1,1),(1,6),(6,1),(6,6), (2,2),(2,5), (3,3),(3,4), (4,3),(4,4),
                     (5,2),(5,5)]]
          relst = sort $ [[1,6], [2,5],[3,4]]
          reduc = toSetDFA [0,1,2] 
                      "ab"
                      0
                      [0] 
                      [(0,'a',0),(0,'b',1),(1,'a',1),(1,'b',2),
                                          (2,'a',2),(2,'b',0)]

--------------------------------------------------------------------------------
-- | Assignment DFA
dfa1 :: DFA
dfa1 = toSetDFA states alpha init final trans 
    where states = [1,2,3]
          alpha  = "abc"
          init   = 1
          final  = [3]
          trans  = [(1,'a',3),(1,'b',2),(2,'a',2),(2,'c',3)]

dfa1_suite :: DFASuite
dfa1_suite = DFASuite org reach fdef indist relst reduc
    where org = dfa1
          reach = toSetDFA [1,2,3] "abc" 1 [3] [(1,'a',3),(1,'b',2),(2,'a',2),(2,'c',3)]
          fdef = toSetDFA [1,2,3,4] "abc" 1 [3] [(1,'a',3),(1,'b',2),(1,'c',4),(2,'a',2),
                                            (2,'b',4),(2,'c',3),(3,'a',4),(3,'b',4),
                                            (3,'c',4),(4,'a',4),(4,'b',4),(4,'c',4)]
          indist = [[(3,3),(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(4,1),(4,2),(4,4)],
                    [(3,3),(1,1),(2,2),(4,4)],
                    [(3,3),(1,1),(2,2),(4,4)]]
          relst = sort $ [[3],[1],[2],[4]]
          reduc = toSetDFA [0,1,2,3] "abc" 0 [1] [(0,'a',1),(0,'b',2),(0,'c',3),(1,'a',3),
                                                  (1,'b',3),(1,'c',3),(2,'a',2),(2,'b',3),
                                                  (2,'c',1),(3,'a',3),(3,'b',3),(3,'c',3)]

dfa2 :: DFA
dfa2 = toSetDFA states alpha init final trans 
    where states = [0,1,2,3,4,5]
          alpha  = "ab"
          init   = 0
          final  = [2,4]
          trans  = [(0,'a',1),(0,'b',1),(1,'a',5),(1,'b',2),(2,'a',3),(2,'b',3),
                    (3,'a',4),(3,'b',5),(4,'a',5),(4,'b',5)]

dfa2_suite :: DFASuite
dfa2_suite = DFASuite org reach fdef indist relst reduc
    where org = dfa2
          reach = dfa2
          fdef = DFA (Set.fromList [0,1,2,3,4,5,6]) (alpha dfa2) (0) (final dfa2)
                 (Set.fromList [(0,'a',1),(0,'b',1),(1,'a',5),(1,'b',2),(2,'a',3),
                                (2,'b',3),(3,'a',4),(3,'b',5),(4,'a',5),(4,'b',5),
                                (5,'a',6),(5,'b',6),(6,'a',6),(6,'b',6)])
          indist = [[(2,2),(2,4),(4,2),(4,4), (0,0),(0,1),(0,3),(0,5),(0,6),(1,0),
                     (1,1),(1,3),(1,5),(1,6),(3,0),(3,1),(3,3),(3,5),(3,6),(5,0),
                     (5,1),(5,3),(5,5),(5,6),(6,0),(6,1),(6,3),(6,5),(6,6)],
                    [(2,2),(2,4),(4,2),(4,4), (0,0),(0,5),(0,6),(1,1), (3,3),(5,0),
                     (5,5),(5,6),(6,0),(6,5),(6,6)],
                    [(2,2),(4,4),(0,0),(1,1),(3,3),(5,5),(5,6),(6,5),(6,6)],
                    [(2,2),(4,4),(0,0),(1,1),(3,3),(5,5),(5,6),(6,5),(6,6)]]
          relst = sort $ [[0],[1],[2],[3],[4],[5,6]]
          reduc = toSetDFA [0,1,2,3,4,5] "ab" 0 [3,5] [(0,'a',1),(0,'b',1),(1,'a',2),
                                                       (1,'b',3),(2,'a',2),(2,'b',2),
                                                       (3,'a',4),(3,'b',4),(4,'a',5),
                                                       (4,'b',2),(5,'a',2),(5,'b',2)]

-- | Custom Benchmark DFA
benchSt :: [State]
benchSt = [0..100]

benchAl :: [Symbol]
benchAl = ['a'..'h']

benchFl :: [State]
benchFl = [0..10]
benchTs :: [Trans]
benchTs = [(p,a,q) | p <- benchSt, q <- benchSt, p <= q, a <- benchAl]

dfaBench :: DFA
dfaBench = toSetDFA states alpha init final trans 
    where states = benchSt
          alpha  = benchAl
          init   = 0
          final  = benchFl
          trans  = benchTs


-- =============================================================================
-- | Functions to be tested

runTests :: Either String String
runTests 
    | minimizeDFA dfaBench == dfaBench = Left "Benchmark error"
    | otherwise = Right "All Ok"

runSuites :: [Either String String]
runSuites = map (uncurry testDFA) suites
    where suites = [(dfa0_suite, "DFA 0: "),
                    (dfa1_suite, "DFA 1: "),
                    (dfa2_suite, "DFA 2: ")]

main :: IO ()
main = do
    putStrLn "Running unit tests ..."
    mapM_ (either (putStrLn . ("Error: " ++)) putStrLn) runSuites 
    either (putStrLn . ("Error: " ++)) putStrLn runTests
    return ()

--------------------------------------------------------------------------------
testDFA :: DFASuite -> String -> Either String String
testDFA suite@DFASuite{..} name
    | resReachable /= reachable
        = showDiff "Remove unreachable" reachable resReachable
    | resFDef /= fdefined 
        = showDiff "Fully defined DFA" fdefined resFDef
    | resIndist0 /= head indistRel
        = showDiff "Indist. relation 0" (head indistRel) resIndist0
    | resIndist /= indistRel
        = showDiff "Indist. relations" indistRel resIndist
    | resRelStates /= relStates
        = showDiff "List of states in relation" relStates resRelStates
    | resReduced /= reduced 
        = showDiff "Reduced DFA" reduced resReduced
    | otherwise = Right (name ++ "OK")
    where resReachable = rmUnreachable dfa
          resFDef = toFullyDefinedDFA resReachable
          resIndist0 = indist0Rel (stateList resFDef) (finalList resFDef)
          resIndist = allIndistRel resFDef
          resRelStates = sort $ statesInRelation (last resIndist) (stateList resFDef) 
          resReduced = toReducedDFA resFDef
          showDiff desc ok bad = Left (name ++ desc ++ "\n" ++ show ok ++ "\n" ++ show bad)

-- | Return the list of indistinguishable relation is the same for all values of k
allIndistRel :: DFA -> [[StatePair]]
allIndistRel dfa@DFA{..} = untilSameIndist [] indist0 (alphaList dfa) (toTransFnc trans)
    where indist0 = indist0Rel (stateList dfa) (finalList dfa)

untilSameIndist :: [StatePair] -> [StatePair] -> [Symbol] -> TransFnc -> 
                   [[StatePair]]
untilSameIndist indist0 indist1 alpha tf
    | indist0 /= indist1 = indist1 : untilSameIndist indist1 indistNext alpha tf
    | otherwise          = [indist1] 
    where indistNext = nextIndist indist1 alpha tf
