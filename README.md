# Minimize-DFA

A command line program for minimization of Deterministic Finite Automaton (DFA),
converts DFA to Minimal Finite Automaton (MFA).


## Usage
$ dka-2-mka option [input]

input   Name of the input file (if not specified, then reads from STDIN)  
        that consits of Deterministic Finite Automaton (DFA) in the following format:

<states>            List of states, each non-negative integer, separated by comma.
<alphabet>          Input alphabet, subset of lowercase ASCII characters [a-z].
<initial state>     The initial state.
<final states>      List of final states.
[transition rule 1] Transition rules in format:
...                     from_state,using_symbol,to_state
[transition rule n] Each rule separated by newline.

option: 
    -i  The input DFA is parsed, and printed out to STDOUT. 
        Can fail if not syntactically correct.
    -t  Prints the MFA to STDOUT with the same format (syntax) as the input DFA,
        but with rules specified in Section "Output MFA".


## Rules

### General
1. Only one option can be specified at a time. 

### Input DFA:
1. <states> is considered a non-empty, finite set, therefore can consist of  
   duplicate states.
2. <alphabet> is considered a non-empty, finite set, therefore can consist of
    duplicate symbols.
3. <initial state> must be defined.
4. <final states> can be empty, in that case it is just an empty line.
5. Transition rules are considered as a finite set (duplicate transitions are 
    skipped), that can be omitted.

### Output MFA
1. <states> are sorted, from zero ascending, continously.
2. Symbols of <alphabet> are sorted.
3. <initial state> is always 0.
4. <final states> are sorted, in ascending order.
5. Transition rules are sorted lexicographically by the "source state", and
   the "symbol used". 
6. A "destination state" of the FIRST transition rule, can be only 0 or 1. 
   "Destination state" of any next rule, can be at most higher by 1
   than any previous (in any rule above) highest state (i.e. having the largest 
   value so far).



Neni treba odchytavat vyjimky


## TODO
Add minimization tests to all OK 
add mix

1. Make rules:
    a. write tests
    b. write readme
    c. write functions with comments

2. run tests and see everything fail
3. implement step by step, until all passes
4. refactor optimize



## Tests

Format: testX.E
where X is an integer starting from 0
E is an ending:
* .in : the input
* .out : what the output SHOULD be, empty if any error
* .temp : output the binary generated
* . err : Error output of the binary

Each test is either correct, then 0 return value is expected and outputs are 
compared, or incorrect (syntactically or others) then only return value 1 is
expected, no output is compared.



## Tips
* Only main module has actions with side effects, others just pure functions

* Use Functions:
** first
** either
** Left
** Right
** find from Data.Foldable
** left
** right
** nothing . .
** listToMaybe

* die is cleaner than error 
* error: partial with undefined value,
*      used in cases that are not reachable, or should not happen

* for bonus: well used library