#!/bin/bash

export dirname
export domname
export problem
export pnum
export probname
export config
export solver
export time
export mem

echorun(){
    # echo $*
    $@
}

wrap (){                        # lisp hack !!!
    echo -n "("
    $@
    echo -n ")"
}

export -f echorun
export -f wrap

main (){
    time mapdir mapprob mapconf $@
}

mapdir (){
    for dirname in $(ls -d */)
    do
        dirname=$(readlink -ef $dirname) # no trailing slash
        domname=$(basename $dirname)
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

