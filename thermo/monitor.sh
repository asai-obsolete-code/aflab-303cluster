#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)

d=/home/asai/Dropbox/room313-thermo-log
mkdir -p $d
cd $d
$SCRDIR/thermo/log-temparatures.sh
sleep 5
$SCRDIR/thermo/graph.ros "./*.log" funcluster-$(date +%m-%d).pdf

