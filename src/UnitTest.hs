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

import Types
import Minimize

--------------------------------------------------------------------------------
-- | Setup of the data to be tested and results of the tests

-- | Reference DFA
dfa0 :: DFA
dfa0 = DFA states alpha init final trans 
    where states = [1,2,3,4,5,6]
          alpha = "ab"
          init = 1
          final = [1,6]
          trans = [(1,'a',6),(1,'b',2),(2,'a',5),(2,'b',4),(3,'a',3),(3,'b',6),
                   (4,'a',4),(4,'b',1),(5,'a',2),(5,'b',3),(6,'a',1),(6,'b',5)]

-- | After removing unreachable states
dfa0_reachable :: DFA
dfa0_reachable = dfa0

-- | Fully defined 
dfa0_fdefined = dfa0

-- | List of indistinguishable relations
dfa0_indist :: [[(State, State)]]
dfa0_indist =  [[(1,1),(1,6),(6,1),(6,6), (2,2),(2,3),(2,4),(2,5), 
                 (3,2),(3,3),(3,4),(3,5), (4,2),(4,3),(4,4),(4,5),
                 (5,2),(5,3),(5,4),(5,5)],
                [(1,1),(1,6),(6,1),(6,6), (2,2),(2,5), (3,3),(3,4), (4,3),(4,4),
                 (5,2),(5,5)],
                [(1,1),(1,6),(6,1),(6,6), (2,2),(2,5), (3,3),(3,4), (4,3),(4,4),
                 (5,2),(5,5)]
               ]

-- | Reduced reference DFA
dfa0_red :: DFA
dfa0_red  = DFA states alpha init final trans 
    where states = [0,1,2]
          alpha = "ab"
          init = 0
          final = [0]
          trans = [(0,'a',0),(0,'b',1),(1,'a',1),(1,'b',2),(2,'a',2),(2,'b',0)]

--------------------------------------------------------------------------------
-- | Functions to be tested


runTests :: Either String String
runTests 
    | rmUnreachable dfa0 /= dfa0 = Left "Remove unreachable states"
    | otherwise = Right "All Ok"

main :: IO ()
main = do
    putStrLn "Running unit tests ..."
    putStrLn $ show $ rmUnreachable dfa0
    either (putStrLn . ((++) "Error: ")) (putStrLn) runTests
    return ()
    

