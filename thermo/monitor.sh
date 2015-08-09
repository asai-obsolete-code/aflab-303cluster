#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)

d=/home/asai/Dropbox/room313-thermo-log
mkdir -p $d
cd $d
$SCRDIR/thermo/log-temparatures.sh
sleep 5
$SCRDIR/thermo/graph.ros "./*-latest.log" funcluster-latest.pdf

cp funcluster-latest-24hrs.pdf funcluster-$(date +%m-%d).pdf
chown asai:asai ./*
