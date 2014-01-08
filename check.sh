#!/bin/bash

failed=0

for x
do
    echo -n "Compiling $x..."
    if ghc --make -Wall -O2 $x.hs -o $x >/dev/null
    then
        echo " [DONE]"
        ret=$(grep "^$x;" solutions.csv | cut -d ";" -f 2)
        tmpfile=$(mktemp)
        ans=$(/usr/bin/time -f "user:%U sys:%S real:%E" -o $tmpfile ./$x)
        if [[ "$ret" != "$ans" ]]
        then
            echo "$x: FAILED"
            echo "EXPECTED : $ret"
            echo "RECEIVED : $ans"
            failed=$(($failed+1))
        else
            echo "$x: OK"
        fi
        echo -n "TIME     : " 
        cat $tmpfile
        echo
    else
        echo " [FAILED]"
        echo "$x: FAILED"
        echo ">>COMPILER ERROR!"
        failed=$(($failed+1))
    fi
done

echo $failed "total fails"
