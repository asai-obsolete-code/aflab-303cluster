#!/bin/bash

dir=$(dirname $(readlink -ef $0))
cg=/sys/fs/cgroup
mem=250000                   # 250MB
maxmem=2000000                  # 2GB: *8
maxcpu=$(cat /proc/cpuinfo | grep "physical id" | wc -l) # physical cpu
time=225                     # 3.75 min = 30/4
maxtime=1800                    # 30 min
debug=false
cgname=

while getopts ":g:P:m:M:t:T:d-" opt
do
    case ${opt} in
        g) cgname=${OPTARG} ;; # group name
        m) mem=${OPTARG:=$mem} ;;
        M) maxmem=${OPTARG:=$maxmem} ;;
        t) time=${OPTARG:=$time} ;;
        T) maxtime=${OPTARG:=$maxtime} ;;
        P) maxcpu=${OPTARG} ;;
        d) debug=true ;;
        -) break;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

if [[ $cgname == "" ]]
then
    echo "ERROR: cgroup name is not specified" >&2
    echo "ERROR: defaulting to '$(whoami)'" >&2
    cgname=$(whoami)
fi 
echo "Ensuring cgroup $cgname exists."
cgcreate -t $(whoami):$(whoami) -a $(whoami):$(whoami) -g cpuacct,memory:$cgname  # might complain, but usually it is successful

################################################################ 
#### parsing qsub options
#### (separated by -- from doubling-qsub options)

optfile_temp=$(mktemp)

while [[ $2 != "" ]]
do
    echo -n "$1 " >> $optfile_temp
    shift
done

command=$(readlink -ef $1)
. utilities.sh

optfile=$command.optfile        # this is shared among nfs
cp $optfile_temp $optfile

next
