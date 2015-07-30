#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)
HOSTS=$SCRDIR/hosts.$1

pbsnodes -o $(sed $HOSTS -e 's/#.*//g')

# wait for jobs to finish
while :
do
    pbsnodes $(sed $HOSTS -e 's/#.*//g') | grep "^\\s*jobs" || break
    sleep 1m
done

# wait for NFS sync
sleep 5m

$SCRDIR/map -h $HOSTS shutdown -P now
