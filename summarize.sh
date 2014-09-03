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
export filter

map (){
    if [[ $2 != "" ]]
    then
        local pred=$1
        local first=$2
        shift 2
        echo $($pred $first)
        map $pred $@
    fi
}

reduce (){                         # list of numbers
    if [[ $3 == "" ]]
    then
        echo $2
    else
        local pred=$1
        local first=$2
        local second=$3
        shift 3
        local next=$($pred $first $second)
        reduce $pred $next $@
    fi
}

gensym (){ mktemp --tmpdir lambda.XXXXXXXX; }
localdef (){ echo "$1=$2;" ;}
localdefs (){
    local i=0
    for arg in $@
    do
        i=$(( $i + 1 ))
        localdef $arg "\$$i"
    done
}
lambda (){
    local sym=$(gensym)
    local args=
    while [[ $1 != "--" ]]
    do
        args="$args $1"
        shift
    done
    shift
    echo "#!/bin/bash" > $sym
    localdefs $args >> $sym
    echo "{ $* ;}" >> $sym
    # echo "defining lambda in $sym:" >&2
    # cat $sym >&2
    chmod +x $sym
    echo $sym
}

gt (){ if [[ $1 -gt $2 ]] ; then echo $1 ; else echo $2 ; fi }
lt (){ if [[ $1 -lt $2 ]] ; then echo $1 ; else echo $2 ; fi }
ge (){ if [[ $1 -ge $2 ]] ; then echo $1 ; else echo $2 ; fi }
le (){ if [[ $1 -le $2 ]] ; then echo $1 ; else echo $2 ; fi }
max (){ reduce gt $@ ;}
min (){ reduce lt $@ ;}

countline (){
    wc -l < $1
}

echorun(){
    # echo $*
    $@
}

wrap (){                        # lisp hack !!!
    echo -n "("
    ## echo $* >&2 # for debugging
    eval $*
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
        $@
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
        $@
    done
}

# p01.pddl
# p01.lmcut-1800-2000000.err

mapconf (){
    for config in $(ls $probname.*.err | sed -e "s/$probname\.\(.*\)\.err/\1/g")
    do
        solver=$(echo $config | sed -e "s/\(.*\)-\([0-9]*\)-\([0-9]*\)/\1/g")
        time=$(echo $config | sed -e "s/\(.*\)-\([0-9]*\)-\([0-9]*\)/\2/g")
        mem=$(echo $config | sed -e "s/\(.*\)-\([0-9]*\)-\([0-9]*\)/\3/g")
        if [[ $config =~ $filter ]]
        then
            $@
        fi
    done
}


