#!/bin/bash

if [[ -f ./hosts ]]
then
    echo Found a local hosts file
    HOSTSFILE=./hosts
elif [[ -f $(dirname $0)/hosts ]]
then
    echo Found a script-local hosts file
    HOSTSFILE=$(dirname $0)/hosts
else
    echo Using the defauilt hosts file
    HOSTSFILE=$(dirname $(readlink -ef $0))/hosts
fi

