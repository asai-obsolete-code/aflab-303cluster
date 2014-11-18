#!/bin/bash

. summarize-util.sh

val=~/repos/downward/src/validate

i=0
max=50
validate (){

    # dirname
    # domname
    # problem
    # pnum
    # probname
    # config
    # solver
    # time
    # mem
    # log
    # err
    # stat
    # elapsed -- elapsed time
    # usage -- memory usage
    # length -- min plan length

    if [[ -e domain.pddl ]]
    then
        domain=domain.pddl
    else
        domain=$probname-domain.pddl
    fi

    for plan in $probname.$config.plan*
    do
        if $val $domain $problem $plan &> /dev/null
        then
            echo -n .
            i=$(( $i + 1 ))
        else
            echo "$domname $plan"
            i=0
        fi
        if [[ $i -gt $max ]]
        then
            echo
            i=0
        fi
    done
}



mapdir mapprob mapconf validate

grep -r "debugger" .
