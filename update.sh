#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0))

$SCRDIR/map -l "cd /root/aflab-303cluster && git pull"
