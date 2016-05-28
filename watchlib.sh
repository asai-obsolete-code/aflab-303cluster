#!/bin/bash

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
    for h in $((pbsnodes -l free ; pbsnodes -l job-exclusive) | awk '{print $1}')
    do
        jobs=$(pbsnodes $h | grep "jobs = " | tr -cd / | wc -c)
        state=$(pbsnodes $h | awk '/state/{print $3; exit 0}')

        withwidth 20 "${h:$(($(echo $h|wc -c)-14)):14}(${state:0:4}):"
        echo -n "["
        withwidth 18 "$(printdots $jobs 18)"
        echo -n "] "
        freetext=$(ssh -o "ConnectTimeout 1" $h free -h | awk '/-\/\+/{printf("U+F:%s+%s",$3,$4)}')
        withwidth 13 "$freetext"
        uptime=$(echo "Ld:" ; ssh -o "ConnectTimeout 1" $h uptime | sed 's/.*average:\(.*\)/\1/g')
        withwidth 14 "$uptime"
        echo
    done
}

job-status (){
    qstat -B
    qstat -rln1 | tail -n +4
}
