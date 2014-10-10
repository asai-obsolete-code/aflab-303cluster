#!/bin/bash

watch -d -t "( ( qstat -q | tail -n 5 | head -n 3 | grep -v '\-\{3\}' ) ; ( qstat -a | grep batch | grep R | head -n 48 ) ; ( pbsnodes | grep '^fun\|jobs =' ) )"
