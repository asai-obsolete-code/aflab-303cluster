#!/bin/bash

scrdir=$(readlink -ef $(dirname $0)/../)

$scrdir/map -l -p -h $scrdir/hosts.room313 "$scrdir/thermo/per-node.sh"

# $scrdir/map -l -h $scrdir/hosts.room313 "sensors ; smartctl -A /dev/sda | grep Temperature_Celsius"

getjobs(){
    qstat -B -f | grep total_jobs | awk '{print $3}'
}

echo "funlucy jobs torque \"$(date -R)\" $(date +%s) $(getjobs)" >> funlucy.log


