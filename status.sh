#!/bin/bash

pbsnodes | while read line
do
    case $line in
        state*) echo -n "($line)" | sed -e 's/state = \(.*\)/\1/g'  ;;
        jobs*)  echo -n ": $line" | sed -e 's/jobs = \(.*\)/\1/g' ;;
        *=*) ;;
        [a-zA-Z0-9]*) echo ; echo -n $line ;;
    esac
done
echo
qstat -q | tail -n 5 | head -n 3 | grep -v '\-\{3\}'
qstat -a | grep batch | grep R
