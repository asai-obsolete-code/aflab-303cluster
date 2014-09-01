#!/bin/bash

export track
export dirname
export problem

echorun(){
    # echo $*
    $@
}

wrap (){                        # lisp hack !!!
    echo -n "("
    $@
    echo -n ")"
}

binaries="template-lmcut"

main (){
    for binary in $binaries
    do
        wrap parbinary $binary > $binary.summary &
    done
    wait
}

parbinary (){
    echo $binary
    for dirname in $(ls -d */)
    do
        if [[ $problem =~ .*planner-scripts.* ]]
        then
            echo "ignoring $problem ..."
            continue
        fi
        pushd $dirname &> /dev/null
        wrap pardir $dirname | tee $binary.summary
        popd &> /dev/null
    done
}

pardir (){                      # or par domain
    # $1 : dirname
    basename $dirname
    find -regex ".*/p[0-9]+\.pddl" | \
        while read problem ; do
        ( echo $(basename $problem .pddl) | cut -c2- ) ; done | \
        sort -g | \
        while read problem ;
    do
        problem=$dirname/p$problem.pddl
        wrap parproblem $problem | tee $(name $problem).$binary.summary
    done
}

name (){
    basename $1 .pddl
}

parproblem (){
    name=$(name $1)
    log=$name.$binary.log
    err=$name.$binary.err
    lbound=$(grep "f = " $log | tail -n 1 | sed -e 's/f = \([0-9.]*\) \[.*$/\1/g')
    echo -n "$name ${lbound:=-1}"
}

time main 2> /dev/null
