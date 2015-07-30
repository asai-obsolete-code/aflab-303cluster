#!/bin/bash

scrdir=$(readlink -ef $(dirname $0)/../)

$scrdir/map -l -h $scrdir/hosts.room313 "$scrdir/thermo/per-node.sh"

# $scrdir/map -l -h $scrdir/hosts.room313 "sensors ; smartctl -A /dev/sda | grep Temperature_Celsius"

