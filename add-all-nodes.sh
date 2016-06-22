#!/bin/bash -x

log=$(ls /var/spool/torque/server_logs/* | sort -h | tail -n 1)

grep "svr_is_request" $log | \
    cut -d";" -f6 | \
    awk '{print $7}' | \
    cut -d: -f1 | \
    sed 's/\./-/g' | \
    awk '{print "ip-"$0}' | sort | uniq | \
    while read line ; do
    qmgr -c "create node $line np=18"
done
