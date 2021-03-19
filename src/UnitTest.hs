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
import Minimize

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
                         , indistRel :: [[(State, State)]]
                         , relStates :: [States]
                         , reduced   :: DFA
                         }

--------------------------------------------------------------------------------
-- | Reference DFA
dfa0 :: DFA
dfa0 = DFA states alpha init final trans 
    where states = [1,2,3,4,5,6]
          alpha = "ab"
          init = 1
          final = [1,6]
          trans = [(1,'a',6),(1,'b',2),(2,'a',5),(2,'b',4),(3,'a',3),(3,'b',6),
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
          relst = [[1,6], [2,5],[3,4]]
          reduc = DFA [0,1,2] "ab" 0 [0] [(0,'a',0),(0,'b',1),(1,'a',1),(1,'b',2),
                                          (2,'a',2),(2,'b',0)]

--------------------------------------------------------------------------------
-- | Assignment DFA
dfa1 :: DFA
dfa1 = DFA states alpha init final trans 
    where states = [1,2,3]
          alpha = "abc"
          init = 1
          final = [3]
          trans = [(1,'a',3),(1,'b',2),(2,'a',2),(2,'c',3)]

dfa1_suite :: DFASuite
dfa1_suite = DFASuite org reach fdef indist relst reduc
    where org = dfa1
          reach = DFA [1,2,3] "abc" 1 [3] [(1,'a',3),(1,'b',2),(2,'a',2),(2,'c',3)]
          fdef = DFA [1,2,3,4] "abc" 1 [3] [(1,'a',3),(1,'b',2),(1,'c',4),(2,'a',2),
                                            (2,'b',4),(2,'c',3),(3,'a',4),(3,'b',4),
                                            (3,'c',4),(4,'a',4),(4,'b',4),(4,'c',4)]
          indist = [[(3,3),(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(4,1),(4,2),(4,4)],
                    [(3,3),(1,1),(2,2),(4,4)],
                    [(3,3),(1,1),(2,2),(4,4)]]
          relst = [[3],[1],[2],[4]]
          reduc = DFA [0,1,2] "abc" 0 [2] [(0,'a',2),(0,'b',1),(1,'a',1),(1,'c',2)]

-- =============================================================================
-- | Functions to be tested

runTests :: Either String String
runTests 
    | otherwise = Right "All Ok"

runSuites :: [Either String String]
runSuites = map (\(d, n) -> testDFA d n) suites
    where suites = [(dfa0_suite, "DFA 0: "),
                    (dfa1_suite, "DFA 1: ")]

main :: IO ()
main = do
    putStrLn "Running unit tests ..."
    mapM (either (putStrLn . ((++) "Error: ")) (putStrLn)) runSuites 
    either (putStrLn . ((++) "Error: ")) (putStrLn) runTests
    return ()
    

--------------------------------------------------------------------------------
testDFA :: DFASuite -> String -> Either String String
testDFA suite@DFASuite{..} name
    | resReachable /= reachable
        = showDiff "Remove unreachable" reachable resReachable
    | resFDef /= fdefined 
        = showDiff "Fully defined DFA" fdefined resFDef
    | resIndist0 /= indistRel !! 0 
        = showDiff "Indist. relation 0" (indistRel !! 0) resIndist0
    | resIndist /= indistRel
        = showDiff "Indist. relations" indistRel resIndist
    | resRelStates /= relStates
        = showDiff "List of states in relation" relStates resRelStates
    | resReduced /= reduced 
        = showDiff "Reduced DFA" reduced resReduced
    | otherwise = Right (name ++ "OK")
   -- | cmpShowDiff "Remove unreachable" reachable resReachable 
    where resReachable = rmUnreachable dfa
          resFDef = toFullyDefinedDFA resReachable
          resIndist0 = indist0Rel (states resFDef) (final resFDef)
          resIndist = allIndistRel resFDef
          resRelStates = indistToStates (last resIndist)
          resReduced = rmSink $ toReducedDFA resFDef
          showDiff desc ok bad = Left (name ++ desc ++ "\n" ++ show ok ++ "\n" ++ show bad)

-- | Return the list of indistinguishable relation is the same for all values of k
allIndistRel :: DFA -> [[(State, State)]]
allIndistRel dfa@DFA{..} = untilSameIndist [] (indist0Rel states final) alphabet trans

untilSameIndist :: [(State, State)] -> [(State, State)] -> Alphabet -> TransRules -> 
                   [[(State, State)]]
untilSameIndist indist0 indist1 alpha trans
    | indist0 /= indist1 = indist1 : untilSameIndist indist1 indistNext alpha trans
    | otherwise          = indist1 : []
    where indistNext = nextIndist indist1 alpha trans

--------------------------------------------------------------------------------
-- | 


