#!/bin/bash


. $(dirname $(readlink -ef $0))/util/gethosts.sh
hosts=$(cat $HOSTSFILE | sed -e "s/#.*//g")

printdots (){
    for ((i=0;i<$1;i++))
    do
        echo -n \#
    done
    for ((i=$1;i<$2;i++))       # $2 = max
    do
        echo -n " "
    done
}

echo "              jobs       mem"

for h in $hosts
do
    jobs=$((1+$(pbsnodes $h | grep "jobs = " | tr -cd , | wc -c)))
    state=$(pbsnodes $h | awk '/state/{print $3; exit 0}')
    echo "$h($state): $(printdots $jobs 8) $(ssh $h free -h | awk '/-\/\+/{printf("%s/%s",$3,$4)}')"
done

echo ----------------------------------------------------------------------------------
qstat -a | tail -n +3 | head -n 3
qstat -a | awk '/.*batch.*/{if (match(/R/,$10)) ; { print $0 }}'
