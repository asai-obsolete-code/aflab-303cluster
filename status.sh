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

withwidth 20 ""
withwidth 11 "jobs"
withwidth 12 "mem"
echo

for h in $hosts
do
    jobs=$(pbsnodes $h | grep "jobs = " | tr -cd / | wc -c)
    state=$(pbsnodes $h | awk '/state/{print $3; exit 0}')
    withwidth 20 "$h($state):"
    echo -n "["
    withwidth 8 "$(printdots $jobs 8)"
    echo -n "] "
    withwidth 12 $(ssh $h free -h | awk '/-\/\+/{printf("%s/%s",$3,$4)}')
    echo
done

qstat -a | tail -n +3 | head -n 3
qstat -a | awk '/.*batch.*/{if (match(/R/,$10)) ; { print $0 }}'
