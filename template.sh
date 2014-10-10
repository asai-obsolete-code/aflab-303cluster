#!/bin/bash

# quoted $ signs are evaluated runtime
echo "$(whoami)@$(hostname)"
tmp=\$(mktemp --tmpdir -d doubling.XXXXXXX)

copy(){
    for plan in \$tmp/*.plan* \$tmp/*.log
    do
        planname=\$(basename \$plan)
        cp \$plan $(dirname $problem)/$outname.\${planname##*$probname.}
    done
    rm -r \$tmp
}

trap "copy" EXIT

cp $problem \$tmp/
cp $domain \$tmp/

pushd \$tmp
$binary $options $probname.pddl $domname.pddl
popd


