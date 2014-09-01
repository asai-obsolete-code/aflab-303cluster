#!/bin/bash

binary=$(readlink -ef $1)
options=$2

cat <<EOF
#!/bin/bash

tmp=\$(mktemp --tmpdir -d doubling.XXXXXXX)

copy(){

for plan in \$tmp/*.plan* \$tmp/*.log
do
    planname=\$(basename \$plan)
    cp \$plan $(dirname $problem)/$outname.\${planname##*$probname.}
done
}

trap "copy" EXIT

cp $problem \$tmp/
cp $(dirname $problem)/domain.pddl \$tmp/

pushd \$tmp
$binary $options $probname.pddl domain.pddl
popd
EOF
