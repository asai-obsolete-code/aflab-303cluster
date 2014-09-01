#!/bin/bash

echorun (){
    echo $*
    $@
}

task (){
    find /tmp/ -name $1 | xargs -P 7 -n 20 rm -rf
}

main (){
    task '*.lisptmp.*'
    task 'fastdownward.*'
    task 'component-planner.*'
    task 'macroff.*'
    task 'fd-benchmark.*'
}

time main
echo finished!
