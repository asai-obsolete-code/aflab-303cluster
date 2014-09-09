#!/bin/bash

. $dir/utilities.sh

pcgname=$cgname
cgname=$pcgname/$$            # child cgname
cg=/sys/fs/cgroup
cgcpu=$cg/cpuacct/$cgname
cgmem=$cg/memory/$cgname
mkdir -p $cgcpu
mkdir -p $cgmem

pushd $cgmem &> /dev/null
echo 0 > memory.swappiness
echo 1 > memory.use_hierarchy
popd &> /dev/null

pid=

sleep=10
mykill (){
    ps $1 &> /dev/null && {
        pstree -p -H $1 $1
        vechodo $dir/killall.sh $1 SIGXCPU
        vechodo $dir/killall.sh $1 SIGTERM
        vechodo sleep $sleep
        vecho sleep end
        ps $1 &> /dev/null && {
            vechodo $dir/killall.sh $1 -9
        }
    }
}

twice-time (){
    echo "Current iteration failed! Doubling the time..." >&2
    (
        time=$(( 2 * $time ))
        if [[ $time -gt $maxtime ]]
        then
            echo "Failed, no more iteration!" >&2
        else
            cgname=$pcgname     # restore the parent cgname
            next                    # throw the next job
        fi
    )
}

twice-mem (){
    echo "Current iteration failed! Doubling the memory..." >&2
    ( 
        mem=$(( 2 * $mem ))
        if [[ $mem -gt $maxmem ]]
        then
            echo "Failed, no more iteration!" >&2
        else
            cgname=$pcgname     # restore the parent cgname
            next                    # throw the next job
        fi
    )
}

finalize (){
    local stat=$(eval "echo $pdir/$outname.stat")
    echo "real $cpuusage (msec.)" >> $stat
    echo "maxmem $memusage (kB)" >> $stat
    rmdir $cgcpu
    rmdir $cgmem
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
cgexec -g cpuacct,memory:$cgname $command &
pid=$!

cpuusage=$(($(< $cgcpu/cpuacct.usage) / 1000000))
memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))

while ps $pid &> /dev/null
do
    sleep 1
    cpuusage=$(($(< $cgcpu/cpuacct.usage) / 1000000))
    memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    if [[ $cpuusage -gt ${time}000 ]]
    then
        echo "cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid
        twice-time
        break
    fi
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


