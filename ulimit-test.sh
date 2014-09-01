#!/bin/bash

ulimit -v 900000 
# ulimit -v 1500000 
# indicating ulimit applies only to per-process status

sbcl --dynamic-space-size 1000 --quit &
sbcl --dynamic-space-size 1000 --quit &

ps -o vsz,rss,cmd

wait
