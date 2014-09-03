#!/bin/bash
. utilities.sh


resources="-t 1800 -T 3600 -m 500000 -M 8000000"
run=true
while getopts ":dr:" opt
do
    case ${opt} in
        d)  # increase verbosity: tail -f search.log during the search
            run=false ;; # true
        r)  # resource specification
            resources=${OPTARG} ;;
        \?) OPT_ERROR=1; break ;;
        * ) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

if [[ ( $1 == "" ) || $OPT_ERROR ]]
then
    cat >&2 <<EOF
usage: ./qsub-all.sh
       [-d] -- dry-run
       templatefile
EOF
    exit 1
fi

export generator=$(readlink -ef $1)
export gen_name=$(basename $generator .sh)

for problem in $(find -regex ".*/p[0-9]*.pddl")
do
    if [[ $problem =~ .*planner-scripts.* ]]
    then
        echo "ignoring $problem ..."
        continue
    fi
    export problem=$(readlink -ef $problem)
    export pdir=$(dirname $problem)
    export qsub=${problem%.pddl}.$gen_name.qsub
    export probname=$(basename $problem .pddl)
    export domname=$(basename $pdir | cut -c 1-4)
    export jobname="$gen_name.$probname.$domname.\$time.\$mem"
    export outname="$probname.$gen_name.\$time.\$mem"
    $generator $(readlink -ef $problem) > $qsub
    chmod +x $qsub
    
    script=$(echo \
        ./doubling-qsub.sh $resources -- \
        -N $jobname \
        -e $pdir/$outname.err \
        -o $pdir/$outname.log $qsub )
    echo $script
    if $run
    then
        $script
    fi
done


