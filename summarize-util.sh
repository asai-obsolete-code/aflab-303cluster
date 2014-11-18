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
    for pnum in $(map $(lambda problem -- 'echo $(basename $problem .pddl) | cut -c2- ')\
                      $(find -regex ".*/p[0-9]+\.pddl") | sort -g)
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
        export elapsed=$(grep "Wall time" $log | cut -d " " -f 3)
        export usage=$(grep "^maxmem" $stat | head -n 1 | cut -d " " -f 2)
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

# parproblem-fd (){
#     macrocost=$(grep "Plan cost:" $log | tail -n 1 | cut -d " " -f 3)
#     preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
#     wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
#     echo
# }

# parproblem-ff (){
#     macrocost=$(grep "Plan cost:" $log | tail -n 1 | cut -d " " -f 4)
#     preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
#     wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
#     echo
# }

# parproblem-mv (){
#     macrocost=$(grep "MakeSpan" $log | cut -d " " -f 3)
#     preprocess=$(grep "[0-9.]* seconds of real time" $log | sed -e "s/^ *//g" | cut -d " " -f 1)
#     wrap echo -n $(safe-echo domname probname solver time mem length elapsed usage macrocost preprocess)
#     echo
# }

# parproblem-lmcut (){
#     log=$probname.$config.log
#     err=$probname.$config.err
#     lbound=$(grep "f = " $log | tail -n 1 | sed -e 's/f = \([0-9.]*\) \[.*$/\1/g')
#     echo -n "$probname ${lbound:=-1}"
# }


#### :action-cost domains

val=~/repos/downward/src/validate

getcost (){
    if [[ -e domain.pddl ]]
    then
        domain=domain.pddl
    else
        domain=$probname-domain.pddl
    fi
    if [[ -e $1 ]]
    then
        # echo "$val -S $domain $problem $1" >&2
        echo $($val -S $domain $problem $1 | sed -e "s/[^0-9 ]//g")
    fi
}

metalength (){
    # 1st: for fd,cea, 2nd: for probe, ff
    min $(grep "Plan length:" $log | cut -d ' ' -f 3) \
        $(grep -C 2 "Result:" $log | grep "Plan cost:" | cut -d ' ' -f 4) # double space
}

actioncost (){
    cost=$(min $(map getcost $(ls $probname.$config.plan* 2>/dev/null )))
    metalength=$(metalength)
    if [[ $metalength != "" && $length != "" ]]
    then
        macrousage=$(($length-$metalength))
    else
        macrousage=-1
    fi
    
    forwardmacro=$(grep "macros after filtering" $log | cut -d ' ' -f 1)
    cyclicmacro=$(grep "(CYCLE" $log | wc -l)

    numeval=$(grep "Number of component plan evaluation:" $log | cut -d ' ' -f 6)
    numcomp=$(grep "Number of comparison:" $log | cut -d ' ' -f 4)

    forward=$(grep "Forward-macro computation" $log | cut -d " " -f 3)
    preprocess=$(grep "Preprocessing time" $log | cut -d " " -f 3)

    # basic info
    # memory infomation
    # plan infomation
    # CAP infomation

    wrap echo -n \
        $(safe-echo domname probname solver time mem elapsed preprocess forward usage cost length macrousage metalength forwardmacro cyclicmacro numeval numcomp)
    echo
}

parproblem-std (){
    cost=$(min $(map getcost $(ls $probname.$config.plan* 2>/dev/null )))
    macrousage=0
    metalength=$length
    forwardmacro=0
    cyclicmacro=0
    numeval=0
    numcomp=0
    preprocess=0
    forward=0
    wrap echo -n \
        $(safe-echo domname probname solver time mem elapsed preprocess forward usage cost length macrousage metalength forwardmacro cyclicmacro numeval numcomp)
    echo
}


parse-failed-cost (){
    grep "Plan cost:" $log | cut -d ' ' -f 3
}

parproblem-failed-iteration (){
    cost=$(min $(parse-failed-cost))
    macrousage=0
    length=0
    metalength=$(metalength)
    macrousage=0
    forwardmacro=0
    cyclicmacro=0
    numeval=0
    numcomp=0
    forward=$(grep "Forward-macro computation" $log | cut -d " " -f 3)
    preprocess=$(grep "Preprocessing time" $log | cut -d " " -f 3)
    wrap echo -n \
        $(safe-echo domname probname solver time mem elapsed preprocess forward usage cost length macrousage metalength forwardmacro cyclicmacro numeval numcomp)
    echo
}
