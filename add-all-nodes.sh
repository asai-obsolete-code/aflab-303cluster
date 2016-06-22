#!/bin/bash

log=$(ls /var/spool/torque/server_logs/* | sort -h | tail -n 1)

echo "searching for unknown ips..."

new_ips=$(
    comm -1 -3 \
        <(pbsnodes | grep ^ip- | sort) \
        <({
            grep "svr_is_request" $log | \
                tail -n 100 | \
                cut -d";" -f6 | \
                awk '{print $7}' | \
                cut -d: -f1 | \
                sed 's/\./-/g' | \
                awk '{print "ip-"$0}' | tail -n 10 | sort | uniq
            }))



for ip in $new_ips ; do
    echo "new ip found! : $ip"
    qmgr -c "create node $ip np=18"
done
