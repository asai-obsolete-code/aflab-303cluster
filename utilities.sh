#!/bin/bash

echovar (){
    echo -n $1 " "
    eval "echo \$$1"
}

echodo (){
    echo $*
    $@
}

kv(){
    eval "echo -n \"$1='\$$1'\""
}

kvs(){
    first=true
    for x in $@
    do
        if $first
        then
            first=false
            echo -n "-v "
        else
            echo -n ","
        fi
        kv $x
    done
}

dkv(){                          # debug
    eval "echo -n \"$1=\$$1 \""
}

dkvs(){                         # debug
    for x in $@ ; do dkv $x ; done
}

exportargs="mem maxmem time maxtime maxcpu \
    cgname debug dir command optfile outname"

next(){
    cd $dir
    ppn=$(($mem / ( $maxmem / $maxcpu )))
    if [[ $ppn == 0 ]] ; then ppn=1; fi
    # add 200MB and 300sec room
    echodo qsub -l mem=$((200000 + $mem )),pmem=$((200000 + $mem )) \
        -l walltime=$(( 300 + $time )) \
        -l nodes=1:ppn=$ppn \
        $(kvs $exportargs) \
        $(eval "echo $(< $optfile)") iterator.sh
}
