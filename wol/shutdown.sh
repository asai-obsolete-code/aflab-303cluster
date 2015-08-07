#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)
HOSTS=$SCRDIR/hosts.$1
[ -z $1 ] && { cat <<EOF
Usage: shutdown.sh ROOM [immediate]

ROOM : room313, room502, room313-summer (fun001,fun002).
immediate : true or false. if false, wait for the nodes to stabilize wrto NFS. 
EOF
    exit 1 ;}

immediate=${2:-false}

pbsnodes -o $(sed $HOSTS -e 's/#.*//g')

# wait for jobs to finish
while :
do
    pbsnodes $(sed $HOSTS -e 's/#.*//g') | grep "^\\s*jobs" || break
    sleep 1m
done

# wait for NFS sync
$immediate && sleep 5m

$SCRDIR/map -h $HOSTS shutdown -P now
