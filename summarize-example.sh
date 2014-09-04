#!/bin/bash

#### 
#### Use this file as a template for writing a result summarizer script.
#### This file is originally written for gathering the results of lama planner.

source summarize.sh

# list of useful variables:

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

# p01.lmcut-1800-2000000.log : $log
# p01.lmcut-1800-2000000.err : $err
# p01.lmcut-1800-2000000.stat : $stat
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
# main : map over directories,problems and configurations

parproblem (){
    cost=$(grep "Best solution cost so far" $log | tail -n 1 | sed -e 's/Best solution cost so far: \([0-9.]*\)$/\1/g')
    echo -n "$solver $time $mem $name ${cost:=-1} ${length:=-1} ${elapsed:=-1} ${usage:=-1}"
}

# example:
main $(lambda -- '[[ $solver == lama && $elapsed -lt 1800 ]]' parproblem > total.summary
