#!/bin/bash

./remove-temp.sh
./clean
./qsub-template.sh -r "-P 4 -t 1800 -T 3600 -m 2000000 -M 8000000" lama.sh
./qsub-template.sh -r "-P 4 -t 1800 -T 1800 -m 2000000 -M 2000000" lmcut.sh
