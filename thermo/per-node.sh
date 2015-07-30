#!/bin/bash

reformat_sensors (){
    echo -n "$(hostname) "
    echo -n "$(echo $line | cut -d: -f1 | sed -e 's/[. ]/-/g') "
    echo -n "$1 "
    shift
    echo -n "\"$(date -R)\" $(date +%s) "
    echo "$1 $4 $7" | sed -e 's/[+),C°]//g'
}

adapter=

sensors -A | while IFS=$'\n' read line ; do
    if (echo $line | grep "Core\|DIMM" &>> /dev/null )
    then
        # filename="$(hostname)-$(echo $line | cut -d: -f1 | sed -e 's/[. ]/-/g').log"
        reformat_sensors $adapter $(echo $line | cut -d: -f2-) | tee -a $(hostname).log
    elif [[ ! -z $line ]]
    then
        # this is bus adapter name
        adapter=$line
    fi
done
