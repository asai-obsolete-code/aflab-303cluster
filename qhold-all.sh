#!/bin/bash

query=${1:-".*"}
qhold $(qstat -a | grep batch | grep "$query" | cut -d. -f 1)
