#!/bin/bash

# list of useful variables:

# dirname
# problem
# pnum
# probname
# config
# solver
# time
# mem

# p01.lmcut-1800-2000000.err
#     ^solver
#           ^time
#                ^mem
#     ^^^^^^^^^^^^^^^^^^config
# problem: p01.pddl
# probname: p01
# pnum: 01

# list of functions:

# echorun : echo and do
# wrap : wrap with ()

parproblem (){
    log=$probname.$config.log
    err=$probname.$config.err
    cost=$(grep "Best solution cost so far" $log | tail -n 1 | sed -e 's/Best solution cost so far: \([0-9.]*\)$/\1/g')
    elapsed=$(grep "real" $err | sed -e "s/real \([0-9.]*\)$/\1/g")
    usage=$(grep "maxmem" $err | sed -e "s/maxmem \([0-9.]*\)$/\1/g")
    echo -n "$name ${cost:=-1} ${elapsed:=-1} ${usage:=-1}"
}

# example:
main parproblem > total.summary
