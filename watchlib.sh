#!/bin/bash

. $(dirname $(readlink -ef $0))/util/gethosts.sh
hosts=$(cat $HOSTSFILE | sed -e "s/#.*//g")

printdots (){
    for ((i=0;i<$1;i++))
    do
        echo -n \#
    done
}

withwidth (){
    w=$1
    n=$(($(echo $2 | wc -c)-1))
    c=${3:-" "}
    echo -n $2
    for ((i=$n;i<$w;i++))       # $2 = max
    do
        echo -n "$c"
    done
}

cluster-status (){
    # withwidth 30 ""
    # withwidth 11 "jobs"
    # withwidth 12 "mem"
    # echo
    for h in $hosts
    do
        jobs=$(pbsnodes $h | grep "jobs = " | tr -cd / | wc -c)
        state=$(pbsnodes $h | awk '/state/{print $3; exit 0}')
        withwidth 30 "$h($state):"
        echo -n "["
        withwidth 8 "$(printdots $jobs 8)"
        echo -n "] "
        freetext=$(ssh -o "ConnectTimeout 1" $h free -h | awk '/-\/\+/{printf("used: %s free: %s",$3,$4)}')
        withwidth 25 "$freetext"
        echo
    done
}

job-status (){
    qstat -B
    qstat -rln1 | tail -n +4
}
