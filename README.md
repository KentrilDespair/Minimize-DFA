# Minimize-DFA

A command line program for minimization of __Deterministic Finite Automaton__ (DFA) written in *Haskell*. Converts DFA to __Reduced Deterministic Finite Automaton__ (RDFA) with __total__ transition function, shortly __Minimal Finite Automaton__ (MFA).

## Requirements
* Make
* GHC, version 8+

## Compilation and Tests
Compiling and running the tests is done using the Makefile in the __root
directory__ of this project.  
After successfull compilation the binary is generated in the same directory
as the Makefile, and is named: __dka-2-mka__ .

1. Compile  
$ make

2. Compile and run  
$ make run  
*No arguments are supplied, therefore a __usage message__ (or help message) is*
*printed to STDOUT.*

3. Compile and run tests  
$ make test  
*After successfull compilation tests are run using a shell script in the __test__ 
directory. See Section [Tests](#Tests) for more info about tests.*

4. Compile and run __Unit tests__
$ make unit
*After successfull compilation generates a new binary named __unitTest__ that
runs unit tests.*

## Usage
$ dka-2-mka option [input]

Options:  
* __-i__  The [input DFA](#Syntax-of-DFA) is pre-parsed, saved into an internal 
          representation   
          and printed out to STDOUT. Can fail if not syntactically correct.
* __-t__  Prints the MFA to STDOUT with the same format (syntax) as the input DFA,
          but with additional rules specified in the Section [Output DFA](#Output-MFA).

* __input__  Name of a file, or if not specified, then STDIN is used, that consists 
             of DFA in a format specified in the Section [Input DFA](#Input DFA). 


## Syntax of DFA

An input DFA consists of various attributes on each line separated by newlines:  
```
<states>             List of states
<alphabet>           Input alphabet
<initial state>      Initial state
<final states>       List of final states
[transition rule 1]  Transition rules - the transition function
...
[transition rule n]  
```
where 
- __state__ is a non-negative integer, each separated by comma,
- __symbol__ of the alphabet is a lowercase ASCII character (a-z),
- __alphabet__ is a string of such symbols, 
- __transition rules__ consists of __n__ single rules that denote each element
                       of the transition function, separated by newline, 
                       in the following format:
```
<from_state>,<using_symbol>,<to_state>
```

#### Example of syntactically correct input DFA:
```
1,2,3  
abc  
1  
3  
1,a,3  
1,b,2  
2,a,2 
2,c,3  
```
  
## Rules
* Only one command line option can be specified at a time (along with possible 
  input file).

### Input DFA
1. `<states>` is considered a non-empty, finite set, therefore can consist of  
   duplicate states.
2. `<alphabet>` is considered a non-empty, finite set, therefore can consist of
    duplicate symbols.
3. `<initial state>` must be defined.
4. `<final states>` can be empty, in that case it is just a newline character.
5. __Transition rules__ are considered as a finite set (duplicate transitions are 
    omitted), that is __optional__.
6. Any number of ending newlines (after the last transition rule, or after the 
    `<final states>`) is __ignored__.

### Output MFA 
1. `<states>` are sorted, continously ascending from zero.
2. Symbols of `<alphabet>` are sorted.
3. `<initial state>` is always 0.
4. `<final states>` are sorted in ascending order.
5. __Transition rules__ are sorted lexicographically by the `<from_state>` and
   the `<using_symbol>`. 
6. The `<to_state>` of the __first__ transition rule, can be only 0 or 1. 
   The `<to_state>` of any next rule, can be at most higher by 1
   than any previous (in any rule above) __highest state__ (i.e. having the largest 
   value so far).

## Conversion in general
The final minimal DFA (i.e. RDFA with total transition function) is created
from DFA by: 1) eliminating unreachable states, 2) converting it to fully 
defined DFA, 3) converting it to Reduced DFA.

1. Elimination of unreachable states



2. Conversion to Fully Defined DFA
3. Conversion to Reduced DFA


1. Elimination of unreachable states
2. Conversion to "fully defined" DFA (i.e. w/ total transition function)
    - If the transition function is not total, then the DFA is extended
      by a so-called "SINK" state
3. Conversion to "reduced" DFA using equivalence theorem (classes)
    - If a "SINK" state exists (i.e. a state that is not final, with transitions
      only to itself), then it is removed.


## TODO
Add minimization tests to all OK 
add mix
just one state test
Check that functions are fully defined
Checck fnc args: what where
tuples to (a,a,a)
listss to (x:xs)
TODO write doc - * toReduced uses table k -> v ...
TODO SORT TRANSRULES only using src and symb!!
31  -- TODO dfa@ not needed if not used!!
Option to remove sink


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

