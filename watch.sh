#!/bin/bash

lib=$(dirname $(readlink -ef $0))/watchlib.sh
. $lib

sudo=false

while getopts ":s" opt
do
    case ${opt} in
        s) 
            sudo=true
            read -s -p "sudo password: " password ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

watch=${watch:-true}

if $watch
then
    export watch=false
    watch -d -t $0 $@
else
    cluster-status
    if $sudo
    then
        echo $password | sudo -S bash -c ". $lib ; job-status"
    else
        job-status
    fi
fi
