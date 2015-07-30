#!/bin/bash
SCRDIR=$(readlink -ef $(dirname $0)/../)

MACFILE=$SCRDIR/MAC.$1
HOSTS=$SCRDIR/hosts.$1
if [[ -z $1 ]]
then
    echo "insufficent number of arguments! $0 $@"
    exit 1
fi

gdep=${2:-false}
reset=${3:-false}

export LC_ALL=C

echo "$(date): $0 $@"

for line in $(sed $MACFILE -e 's/#.*//g')
do
    if $gdep
    then
        echo "ssh gdep4core wakeonlan $line"
        ssh gdep4core "LC_ALL=C wakeonlan $line"
    else
        wakeonlan $line
    fi
    sleep 1
done

for fun in $(sed $HOSTS -e 's/#.*//g')
do
    timeout 1h $SCRDIR/wol/setup-node.sh $fun $reset &
done

wait

echo "$(date): $0 $reset finished."
