#!/bin/bash

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
    echo $* >&2
    export pred=$1
    shift
    time ( mapdir mapprob mapconf $@ | sed -e 's/()//g' )
}

mapdir (){
    for dirname in $(ls -d */)
    do
        export dirname=$(readlink -ef $dirname) # no trailing slash
        export domname=$(basename $dirname)
        if [[ $dirname =~ .*planner-scripts.* ]]
        then
            echo "ignoring $dirname ..." >&2
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
        export pnum
        export probname=p$pnum
        export problem=$(readlink -ef $probname.pddl)
        $@
    done
}

# p01.pddl
# p01.lmcut-1800-2000000.err

mapconf (){
    for config in $(map $(lambda x -- 'echo $x | cut -d. -f 2-4') \
                        $(ls $probname.*.err 2>/dev/null ))
    do
        export config
        export solver=$(echo $config | cut -d. -f 1 )
        export time=$(echo $config | cut -d. -f 2 )
        export mem=$(echo $config | cut -d. -f 3 )
        export log=$probname.$config.log
        export err=$probname.$config.err
        export stat=$probname.$config.stat
        export length=$(min $(map countline $(ls $probname.$config.plan* 2>/dev/null )))
        export elapsed=$(grep "^real" $stat | cut -d " " -f 2)
        export usage=$(grep "^maxmem" $stat | cut -d " " -f 2)
        if $pred
        then
            $@
        fi
    done
}

safe-echo (){
    for arg in $@
    do
        eval "echo -n \"\${$arg:=-1} \""
    done
}

################################################################
#### per-planner

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

parproblem-std (){
    wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage)
    echo
}


parproblem-fd (){
    macrocost=$(grep "Plan cost:" $log | tail -n 1 | cut -d " " -f 3)
    preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
    wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
    echo
}

parproblem-ff (){
    macrocost=$(grep "Plan cost:" $log | tail -n 1 | cut -d " " -f 4)
    preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
    wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
    echo
}

parproblem-mv (){
    macrocost=$(grep "MakeSpan" $log | cut -d " " -f 3)
    preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
    wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
    echo
}

parproblem-lmcut (){
    log=$probname.$config.log
    err=$probname.$config.err
    lbound=$(grep "f = " $log | tail -n 1 | sed -e 's/f = \([0-9.]*\) \[.*$/\1/g')
    echo -n "$probname ${lbound:=-1}"
}


#### :action-cost domains

val=~/repos/downward/src/validate

getcost (){
    if [[ -e domain.pddl ]]
    then
        domain=domain.pddl
    else
        domain=$probname-domain.pddl
    fi
    $val -S $domain $problem $1
}

actioncost (){
    cost=$(min $(map getcost $(ls $probname.$config.plan* 2>/dev/null )))
    # macrocost=$(grep "Plan cost:" $log | tail -n 1 | cut -d " " -f 3)
    preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
    wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage 0 preprocess cost)
    echo
}

