#!/bin/bash

parproblem (){
    log=$probname.$config.log
    err=$probname.$config.err
    cost=$(grep "Best solution cost so far" $log | tail -n 1 | sed -e 's/Best solution cost so far: \([0-9.]*\)$/\1/g')
    time=$(grep "real" $err | sed -e "s/real \([0-9.]*\)$/\1/g")
    memory=$(grep "maxmem" $err | sed -e "s/maxmem \([0-9.]*\)$/\1/g")
    echo -n "$name ${cost:=-1} ${time:=-1} ${memory:=-1}"
}

# example:
main parproblem > total.summary
