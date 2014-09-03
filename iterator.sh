#!/bin/bash

. $dir/utilities.sh

ccgname=$cgname/$$            # child cgname
cg=/sys/fs/cgroup
cgcpu=$cg/cpuacct/$ccgname
cgmem=$cg/memory/$ccgname
mkdir -p $cgcpu
mkdir -p $cgmem

pushd $cgmem &> /dev/null
echo 0 > memory.swappiness
popd &> /dev/null

pid=

sleep=10
mykill (){
    echodo kill -s SIGXCPU $1
    sleep $sleep
    ps $1 &> /dev/null && {
        echodo kill -s SIGTERM $1
        sleep $sleep
        ps $1 &> /dev/null && {
            echodo kill -s SIGKILL $1
        }
    }
}

twice-time (){
    echo "Current iteration failed! Doubling the time..." >&2
    local ntime=$(( 2 * $time ))
    if [[ $ntime -gt $maxtime ]]
    then
        echo "Failed, no more iteration!" >&2
    else
        time=$ntime
        next                    # throw the next job
    fi
}

twice-mem (){
    echo "Current iteration failed! Doubling the memory..." >&2
    nmem=$(( 2 * $mem ))
    if [[ $nmem -gt $maxmem ]]
    then
        echo "Failed, no more iteration!" >&2
    else
        mem=$nmem
        next                    # throw the next job
    fi
}

finalize (){
    echo "real $(($(< $cgcpu/cpuacct.usage) / 1000000)) (msec.)" > $outname.stat
    echo "maxmem $(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 )) (kB)" > $outname.stat
    rmdir -f $cgcpu
    rmdir -f $cgmem
}

trap finalize EXIT

for i in {0..20}
do
    if [[ -e $command ]]
    then
        break                   # wait for the nfs synchronization
    fi
    sleep 3
done

export time mem maxtime maxmem maxcpu cgname cgcpu
cgexec -g cpuacct,memory:$ccgname $command &
pid=$!

cpuusage=$(($(< $cgcpu/cpuacct.usage) / 1000000))
memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))

while ps $pid &> /dev/null
do
    sleep 1
    cpuusage=$(($(< $cgcpu/cpuacct.usage) / 1000000))
    if [[ $cpuusage -gt ${time}000 ]]
    then
        echo "cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid
        twice-time
        break
    fi
    memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    if [[ $memusage -gt $mem ]]
    then
        echo "memory.max_usage_in_bytes exceeding. $memusage kB." >&2
        mykill $pid
        twice-mem
        break
    fi
done

wait $pid
exitstatus=$?

case $exitstatus in
    0)
        echo The program successfully finished.
        ;;
    *)
        echo Error occured. status: $exitstatus
        ;;
esac

