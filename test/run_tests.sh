#!/bin/bash

# Return values to be compared for each test
# When test is OK then the outputs are compared, 
#   otherwise only error code is checked.
OK=0
ERR=1

# Default path to binary
BIN="dka-2-mka"

# Help message
_usage="Usage: $0 [binary]
Assumes tests to be in the same directory as this script.

Options:
  binary    Path to binary, if empty then \"./$BIN\" used
"
# ------------------------------------------
# Arguments

# Optional path to binary as 1st argument
if [ ! -z "$1" ] ; then
    BIN="$1"
fi

# Check if binary exists
if [ ! -f $BIN ] ; then
    echo "Error: binary file \"$BIN\" not found."
    echo "$_usage"
    exit 1;
fi

# Directory of tests same as the script's
DIR=$(dirname "$0")

## Names of tests, to assure the right order
declare -a tests=(
    "test0" "test1" "test2" "test3" "test4" "test5" "test6" "test7" "test8" "test9"
    "test10" "test11" "test12" "test13" "test14" "test15" "test16" "test17" "test18"
    "test19" "test20" "test21" "test22" "test23" "test24" "test25" "test26" "test27"
    "test28" "test29" "test30" "test31" "test32" "test33"
)

# For each test define:
# Error code, argument and description
declare -A params=( ["${tests[0]}"]="$OK -t Minimization of the reference DFA"
                    ["${tests[1]}"]="$OK -i Representation of the reference DFA"
                    ["${tests[2]}"]="$OK -t Minimization of the assignment DFA"
                    ["${tests[3]}"]="$OK -i Representation of the assignment DFA"
                    # States
                    ["${tests[4]}"]="$ERR -i States: empty, missing"
                    ["${tests[5]}"]="$ERR -i States: incorrect formatting"
                    ["${tests[6]}"]="$ERR -i States: incorrect syntax"
                    ["${tests[7]}"]="$OK -i States: duplicate states"
                    # Alphabet
                    ["${tests[8]}"]="$ERR -i Alphabet: empty, missing"
                    ["${tests[9]}"]="$ERR -i Alphabet: incorrect formatting"
                    ["${tests[10]}"]="$ERR -i Alphabet: incorrect syntax of symbols"
                    ["${tests[11]}"]="$OK -i Alphabet: duplicate symbols"
                    # Initial state
                    ["${tests[12]}"]="$ERR -i Initial state: missing"
                    ["${tests[13]}"]="$ERR -i Initial state: not in states"
                    ["${tests[14]}"]="$ERR -i Initial state: incorrect syntax"
                    ["${tests[15]}"]="$ERR -i Initial state: multiple"
                    # Final states
                    ["${tests[16]}"]="$ERR -i Final states: not in states"
                    ["${tests[17]}"]="$ERR -i Final states: incorrect syntax"
                    ["${tests[18]}"]="$ERR -i Final states: incorrect formatting"
                    ["${tests[19]}"]="$OK -i Final states: empty"
                    ["${tests[20]}"]="$OK -i Final states: duplicate states"
                    # Transition rules
                    ["${tests[21]}"]="$ERR -i Transition rules: src states not in states"
                    ["${tests[22]}"]="$ERR -i Transition rules: dst states not in states"
                    ["${tests[23]}"]="$ERR -i Transition rules: symbols not in alphabet"
                    ["${tests[24]}"]="$ERR -i Transition rules: incorrect formatting"
                    ["${tests[25]}"]="$ERR -i Transition rules: incorrect syntax"
                    ["${tests[26]}"]="$OK -i Transition rules: empty"
                    ["${tests[27]}"]="$OK -i Transition rules: duplicate"
                    # Mix
                    ["${tests[28]}"]="$ERR -i Nondeterministic Finite Automata"
                    ["${tests[29]}"]="$ERR -i Newlines at the end of the file"
                    ["${tests[30]}"]="$ERR -a Unknown argument"
                    ["${tests[31]}"]="$ERR    Missing arguments"
                    ["${tests[32]}"]="$ERR -i Alphabet: unicode lower case symbol"
                    ["${tests[33]}"]="$OK -t Minimization, unreachable states"
)

# Counters for statistics
total=0
total_ok=0
total_err=0


handle_err() {
    echo -e "\e[31mERR\e[39m $t: $desc"
    echo -e "    $BIN $arg < $TIN > $OUT"
    echo -e "\tReturned: $RET, should be: $code"

    ((total_err+=1))
    ((total+=1))
}

handle_not_matching() {
    echo -e "\e[31mERR\e[39m $t: $desc"
    echo -e "    $BIN $arg < $TIN > $OUT"
    echo -e "\tOutputs NOT MATCHING: $TOUT $OUT"

    ((total_err+=1))
    ((total+=1))
}

handle_ok() {
    echo -e "\e[32mOK\e[39m  $t: $desc"
    ((total_ok+=1))
}

# Run all tests
for t in "${tests[@]}"
do
    value=${params[$t]}
    code=${value:0:2}
    arg=${value:2:2}
    desc=${value:5}

    # --------------------------------
    # RUN

    TIN="$DIR/$t.in"
    TOUT="$DIR/$t.out"
    OUT="$DIR/$t.temp"
    EOUT="$DIR/$t.err"

    # TODO
    # Run as a file
    #./$BIN $arg $TIN

    # Run as an input
    ./$BIN $arg < $TIN 1> $OUT 2> $EOUT

    # --------------------------------
    # Evaluate

    RET=$?

    # On Error, no need to compare outputs
    if [ $RET -ne $code ] ; then
        handle_err
        cat $EOUT
        continue
    fi
    # Passed

    # Compare outputs if the test should pass
    if [ $code -ne $ERR ] ; then

        #diff $TOUT $OUT &> /dev/null
        match=$(diff $TOUT $OUT)
        if [ $? -ne 0 ] ; then
            handle_not_matching
            cat $EOUT
            echo "$match"
            continue
        fi
    fi 

    handle_ok
    cat $EOUT
    ((total+=1))
done

echo -e "\e[39mTotal: $total ; Failed: $total_err ; OK: $total_ok"

