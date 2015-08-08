#!/bin/bash

scrdir=$(readlink -ef $(dirname $0)/../)

$scrdir/map -l -p -h $scrdir/hosts.room313 "$scrdir/thermo/per-node.sh"

# $scrdir/map -l -h $scrdir/hosts.room313 "sensors ; smartctl -A /dev/sda | grep Temperature_Celsius"

getjobs(){
    qstat -B -f | grep total_jobs | awk '{print $3}'
}

echo "funlucy jobs torque \"$(date -R)\" $(date +%s) $(getjobs)" >> funlucy-latest.log

cut_log (){
    if [[ $(wc -l < $1) -gt 10600 ]]
    then
        head -n -9600 $1 > ${1%%-latest.log}-$(date --date "last week" +%m-%d).log
        tail -n 9600 $1 > $1.tmp
        rm $1
        mv $1.tmp $1
    fi
}

for log in *-latest.log
do
    cut_log $log
done
