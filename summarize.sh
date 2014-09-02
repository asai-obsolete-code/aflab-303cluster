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

main (){
    time mapdir mapprob mapconf $@
}

mapdir (){
    for dirname in $(ls -d */)
    do
        if [[ $dirname =~ .*planner-scripts.* ]]
        then
            echo "ignoring $dir ..." >&2
            continue
        fi
        pushd $dirname &> /dev/null
        wrap $@
        popd &> /dev/null
    done
}

mapprob (){
    find -regex ".*/p[0-9]+\.pddl" | \
        while read problem ; do
        ( echo $(basename $problem .pddl) | cut -c2- ) ; done | \
            sort -g | \
            while read pnum ;
    do
        probname=p$pnum
        problem=$(readlink -ef $probname.pddl)
        wrap $@
    done
}

# p01.pddl
# p01.lmcut-1800-2000000.err

mapconf (){
    for config in $(ls $probname.*.err | sed -e "s/$probname\.\(.*\)\.err/\1/g")
    do
        solver=$(echo $config | sed -e "s/\(.*\)-\([0^9]*\)-\([0-9]*\)/\1/g")
        time=$(echo $config | sed -e "s/\(.*\)-\([0^9]*\)-\([0-9]*\)/\2/g")
        mem=$(echo $config | sed -e "s/\(.*\)-\([0^9]*\)-\([0-9]*\)/\3/g")
        wrap $@
    done
}

# parproblem (){
#     log=$probname.$config.log
#     err=$probname.$config.err
#     cost=$(grep "Best solution cost so far" $log | tail -n 1 | sed -e 's/Best solution cost so far: \([0-9.]*\)$/\1/g')
#     time=$(grep "real" $err | sed -e "s/real \([0-9.]*\)$/\1/g")
#     memory=$(grep "maxmem" $err | sed -e "s/maxmem \([0-9.]*\)$/\1/g")
#     echo -n "$name ${cost:=-1} ${time:=-1} ${memory:=-1}"
# }

# example:
# main parproblem 2> /dev/null > total.summary
