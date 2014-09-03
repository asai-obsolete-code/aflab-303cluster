#!/bin/bash


outname=$PWD/tarai-\$time-\$mem ./doubling-qsub.sh -d -t 1 -T 100 -m 1000000 -M 1000000 tarai.lisp
# 1MB
