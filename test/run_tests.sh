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

## Names of tests, to assure the order
declare -a tests=("test0" "test1" "test2" "test3" "test4" "test5")

# For each test define:
# Error code, argument and description
declare -A params=( ["${tests[0]}"]="$OK -t Minimization of the reference DFA"
                    ["${tests[1]}"]="$OK -i Representation of the reference DFA"
                    ["${tests[2]}"]="$OK -t Minimization of the assignment DFA"
                    ["${tests[3]}"]="$OK -i Representation of the assignment DFA"
                    ["${tests[4]}"]="$OK -i No transitions"
                    ["${tests[5]}"]="$OK -i Empty final states"
                    #["test6"]="$OK -i Empty final states"
                    #["test3"]="-i $OK No transitions"
                    #["test4"]="-i $OK Empty final states"
                    #["test5"]="-i $ERR No states"
                    #["test6"]="-i $ERR State as word"
                    #["test7"]="-i $ERR State as negative"
                    #["test8"]="-i $ERR State as float"
                    #["test9"]="-i $ERR States not separated"
                    #["test10"]="-i $ERR States separated using blanks"
                    #["test11"]="-i $ERR States no newline"
                    #["test12"]="-i $ERR States wrong ending"
                    #["test13"]="-i $ERR No alphabet"
                    #["test14"]="-i $ERR Alphabet uppercase"
                    #["test15"]="-i $ERR Alphabet Ints"
                    #["test16"]="-i $ERR Alphabet separated"
                    #["test17"]="-i $ERR Alphabet unicode"
                    #["test18"]="-i $ERR No initial state"
                    #["test19"]="-i $ERR Initial state not in states"
                    #["test20"]="-i $ERR Initial state as word"
                    #["test21"]="-i $ERR Initial states"
                    #["test22"]="-i $ERR Initial state as negative"
                    #["test23"]="-i $ERR Final state not in states"
                    #["test24"]="-i $ERR Final state as word"
                    #["test25"]="-i $ERR Final states wrong separated"
                    #["test26"]="-i $ERR Final state negative"
                    #["test27"]="-i $ERR Transition src not in states"
                    #["test28"]="-i $ERR Transition dst not in states"
                    #["test29"]="-i $ERR Transition symbol not in alphabet"
                    #["test30"]="-i $OK Duplicate transition"
                    #["test31"]="-i $ERR NFA transitions"
                    #["test32"]="-i $ERR Transitions wrong seperator"
                    #["test33"]="-i $ERR No final state only comma"
                    #["test34"]="-i $ERR More newlines at the end"
                    #["test35"]="-i $OK Duplicit states"
                    #["test36"]="-i $OK Duplicit alphabet symbols"
)

# Counters for statistics
total=0
total_ok=0
total_err=0


handle_err() {
    echo -e "\e[31mERR\t$t: $desc"
    echo "\t./$BIN $arg < $TIN > $OUT"
    echo "\tReturned: $RET, should be: $code"

    ((total_err+=1))
    ((total+=1))
}

handle_not_matching() {
    echo -e "\e[31mERR\e[39m $t: $desc"
    echo -e "    $BIN $arg < $TIN > $OUT"
    echo -e "\tOutput $TOUT and $OUT not matching"

    ((total_err+=1))
    ((total+=1))
}

handle_ok() {
    echo -e "\e[32mOK\t$t: $desc"
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

    # Run as a file
    #./$BIN $arg $TIN
    # Run as an input
    ./$BIN $arg < $TIN > $OUT

    # --------------------------------
    # Evaluate

    RET=$?

    # On Error, no need to compare outputs
    if [ $RET -ne $code ] ; then
        handle_err
        continue
    fi
    # Passed

    # Compare outputs if the test should pass
    if [ $code -ne $ERR ] ; then

        diff $TOUT $OUT &> /dev/null
        #diff $TOUT $OUT
        if [ $? -ne 0 ] ; then
            handle_not_matching
            continue
        fi
    fi 

    handle_ok
    ((total+=1))
done

echo -e "\e[39mTotal: $total ; Failed: $total_err ; OK: $total_ok"

