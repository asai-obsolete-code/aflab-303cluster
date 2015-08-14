#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)

d=/home/asai/Dropbox/room313-thermo-log
mkdir -p $d
cd $d
$SCRDIR/thermo/log-temparatures.sh
sleep 2
$SCRDIR/thermo/graph.ros
cp funcluster-latest-24hrs.pdf funcluster-$(date +%m-%d).pdf
chown asai:asai ./*
