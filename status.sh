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

withwidth 30 ""
withwidth 11 "jobs"
withwidth 12 "mem"
echo

for h in $hosts
do
    jobs=$(pbsnodes $h | grep "jobs = " | tr -cd / | wc -c)
    state=$(pbsnodes $h | awk '/state/{print $3; exit 0}')
    withwidth 30 "$h($state):"
    echo -n "["
    withwidth 8 "$(printdots $jobs 8)"
    echo -n "] "
    freetext=$(ssh $h free -h | awk '/-\/\+/{printf("used: %s free: %s",$3,$4)}')
    withwidth 25 "$freetext"
    echo
done
qstat -q | awk '/batch/{printf("Running: %s Pending: %s\n",$6,$7)}'
qstat -a | tail -n +3 | head -n 3
qstat -a | awk '/.*batch.*/{if (match(/R/,$10)) ; { print $0 }}'
