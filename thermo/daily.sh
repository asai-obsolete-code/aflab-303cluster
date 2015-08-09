#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)

d1=/home/asai/Dropbox/room313-thermo-log
d2=/home/asai/Dropbox/FukunagaLabShare/room313-thermo-log
mkdir -p $d1 $d2

cp -u -t $d2 $d1/*
chown asai:asai $d2/*
