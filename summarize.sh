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

binaries="template-lama template-lama-long"

main (){
    for binary in $binaries
    do
        wrap parbinary $binary > $binary.summary &
    done
    wait
    cat *.summary
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
    cost=$(grep "Best solution cost so far" $log | tail -n 1 | sed -e 's/Best solution cost so far: \([0-9.]*\)$/\1/g')
    time=$(grep "real" $err | sed -e "s/real \([0-9.]*\)$/\1/g")
    memory=$(grep "maxmem" $err | sed -e "s/maxmem \([0-9.]*\)$/\1/g")
    echo -n "$name ${cost:=-1} ${time:=-1} ${memory:=-1}"
}

time wrap main 2> /dev/null > total.summary
