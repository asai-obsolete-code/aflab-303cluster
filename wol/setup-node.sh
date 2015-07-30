#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)
node=$1
reset=${2:-false}
log (){
    echo "$(date): $node $@"
}

retry_until_success (){
    sleep=$1
    shift
    log attempting $@
    while ! $@
    do
        log retrying $@ --- after $sleep
        sleep $sleep
        log attempting $@
    done
    log finished $@
}

retry_until_success 1m ssh $node true
retry_until_success 1m ssh $node $SCRDIR/wol/cgroup-setup.sh asai
$reset && retry_until_success 1m pbsnodes -r $node

log finished.
