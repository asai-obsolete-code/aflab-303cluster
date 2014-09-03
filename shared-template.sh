#!/bin/bash

if [[ $2 == "" ]]
then
    cat >&2 <<EOF
Usage: shared-template.sh binary options
EOF
    exit 1
fi

binary=$(readlink -ef $1)
options=$2

# writes a qsub script to the standard output
# the script 1. cd to the temp directory
# 2. run $binary
# 3. copy back the result plan/log files to the original directory
#    appending the appropriate name for the job and the configuration

if [[ $problem == "" ]]
then
    echo '$problem not specified!' >&2
    exit 1
fi
if [[ $outname == "" ]]
then
    echo '$outname not specified!' >&2
    exit 1
fi
if [[ $probname == "" ]]
then
    echo '$probname not specified!' >&2
    probname=$(basename $problem)
fi

eval "cat <<EOF
$(< template.sh)
EOF"
