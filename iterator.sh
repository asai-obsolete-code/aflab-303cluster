]#!/bin/bash

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
    echodo kill -s SIGXCPU $pid
    sleep $sleep
    ps $pid &> /dev/null && {
        echodo kill -s SIGTERM $pid
        sleep $sleep
        ps $pid &> /dev/null && {
            echodo kill -s SIGKILL $pid
        }
    }
}

twice-time (){
    echo "Current iteration failed! time=$time, maxtime=$maxtime, Doubling the time..." >&2
    ntime=$(( 2 * $time ))
    if [[ $ntime -gt $maxtime ]]
    then
        echo "Failed, no more iteration!" >&2
        exit 4
    fi
    time=$ntime
    next
}

twice-mem (){
    echo "Current iteration failed! mem=$mem, maxmem=$maxmem, Doubling the memory..." >&2
    nmem=$(( 2 * $mem ))
    if [[ $nmem -gt $maxmem ]]
    then
        echo "Failed, no more iteration!" >&2
        exit 5
    fi
    mem=$nmem
    next
}

finalize (){
    echo
    echo "real $(($(< $cgcpu/cpuacct.usage) / 1000000)) (msec.)"
    echo "maxmem $(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 )) (kB)"
    rmdir $cgcpu
    rmdir $cgmem
    rm -f $finished
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

echo Current resource limit: time: $time memory: $mem
export time mem maxtime maxmem maxcpu cgname cgcpu
finished=$(mktemp)
( cgexec -g cpuacct,memory:$ccgname $command ; echo $? > $finished ) &
pid=$!

case $(< $finished) in
    0)
        echo The program successfully finished
        echo immediately before the resource check loop started.
        ;;
    1)
        echo Error occured. The program finished
        echo immediately before the resource check loop started.
        echo Consider increasing the initial resource limit for the iteration.
        ;;
    *)
        while ps $pid &> /dev/null
        do
            sleep 1
            cpuusage=$(($(< $cgcpu/cpuacct.usage) / 1000000))
            if [[ $cpuusage -gt ${time}000 ]]
            then
                echo "cpuacct.usage exceeding. $cpuusage msec."
                mykill
                twice-time
                exit 2
            fi
            memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
            if [[ $memusage -gt $mem ]]
            then
                echo "memory.max_usage_in_bytes exceeding. $memusage kB."
                mykill
                twice-mem
                exit 3
            fi
        done
        echo Process successfully finished under the limit!
        ;;
esac

