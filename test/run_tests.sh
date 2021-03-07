#!/bin/bash


BIN="dka-2-mka"

OK=0
ERR=1

# Binary name as first argument
if [ ! -z "$1" ] ; then
    BIN="$1"
fi

# With what parameters run all single tests
declare -A params=( ["test01"]="-t $OK Minimization of assignment DFA"
                    ["test02"]="-i $OK Representation of assignment DFA"
                    ["test03"]="-i $OK No transitions"
                    ["test04"]="-i $OK Empty final states"
                    ["test05"]="-i $ERR No states"
                    ["test06"]="-i $ERR State as word"
                    ["test07"]="-i $ERR State as negative"
                    ["test08"]="-i $ERR State as float"
                    ["test09"]="-i $ERR States not separated"
                    ["test10"]="-i $ERR States separated using blanks"
                    ["test11"]="-i $ERR States no newline"
                    ["test12"]="-i $ERR States wrong ending"
                    ["test13"]="-i $ERR No alphabet"
                    ["test14"]="-i $ERR Alphabet uppercase"
                    ["test15"]="-i $ERR Alphabet Ints"
                    ["test16"]="-i $ERR Alphabet separated"
                    ["test17"]="-i $ERR Alphabet unicode"
                    ["test18"]="-i $ERR No initial state"
                    ["test19"]="-i $ERR Initial state not in states"
                    ["test20"]="-i $ERR Initial state as word"
                    ["test21"]="-i $ERR Initial states"
                    ["test22"]="-i $ERR Initial state as negative"
                    ["test23"]="-i $ERR Final state not in states"
                    ["test24"]="-i $ERR Final state as word"
                    ["test25"]="-i $ERR Final states wrong separated"
                    ["test26"]="-i $ERR Final state negative"
                    ["test27"]="-i $ERR Transition src not in states"
                    ["test28"]="-i $ERR Transition dst not in states"
                    ["test29"]="-i $ERR Transition symbol not in alphabet"
                    ["test30"]="-i $OK Duplicate transition"
                    ["test31"]="-i $ERR NFA transitions"
                    ["test32"]="-i $ERR Transitions wrong seperator"
                    ["test33"]="-i $ERR No final state only comma"
                    ["test34"]="-i $ERR More newlines at the end"
                    ["test35"]="-i $OK Duplicit states"
                    ["test36"]="-i $OK Duplicit alphabet symbols"
)

                    # nebo final nedosazitelne


# Check if binary exists
if [ ! -f $BIN ] ; then
    echo "Error, binary file not found in $BIN"
    exit 1;
fi

# Get all test file names
TESTS=$(find . -type f -name '*.in' | sort | rev | cut -c4- | rev)

test_ctr=0
succ_ctr=0
fail_ctr=0
# Run all tests with set parameters
for t in $TESTS
do
    # Get test name
    tname=$(echo $t | rev | cut -d '/' -f 1 | rev)

    param=${params[$tname]:0:2}
    info=${params[$tname]:5}
    ret=${params[$tname]:3:1}

    echo -e "\e[33m$tname: $info"

    # Test without set parameters is run as "-t" by default
    if [ -z "$param" ] ; then
        run_str="$BIN -t"
        info="Unknown test"
        ret=$OK
    else
        run_str="$BIN $param"
    fi

    # INput and result
    in_test="$t.in"
    res_test="$t.res"

    out_test="$t.out"
    err_test="$t.err"

    # Run once as input file and once as STDIN
    # -----------------------------------
    ./$run_str $in_test > $out_test 2> $err_test
    #./$run_str < $in_test > $out_test 2> $err_test

    # Has failed 
    if [ $? -ne 0 ] ; then
        if [ $ret -eq $ERR ] ; then
            echo -e "\e[32mOK"
            ((succ_ctr+=1))
        else
            echo -e "\e[31mFAIL"
            ((fail_ctr+=1))
        fi
    else
        # Was meant to fail
        if [ $ret -eq $ERR ] ; then
            echo -e "\e[31mFAIL"
            ((fail_ctr+=1))
        else 
            diff $res_test $out_test &> /dev/null
            if [ $? -eq 0 ] ; then
                echo -e "\e[32mOK"
                ((succ_ctr+=1))
            else
                echo -e "\e[31mFAIL NOT MATCHING"
                ((fail_ctr+=1))
            fi
        fi
    fi

    ((test_ctr+=1))
done

echo -e "\e[39mTotal: $test_ctr ; Failed: $fail_ctr ; OK: $succ_ctr"
