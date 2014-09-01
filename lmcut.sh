#!/bin/bash


./shared-template.sh \
    planner-scripts/test-problem.sh \
    "-v -t 3600 -m 9000000 -o \"--search 'astar(lmcut())'\""
