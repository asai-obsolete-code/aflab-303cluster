#!/bin/bash

watch "( qstat -q ; ( qstat -a | grep R | head -n 50 ) ; ( pbsnodes | grep '^fun\|jobs =' ) )"
