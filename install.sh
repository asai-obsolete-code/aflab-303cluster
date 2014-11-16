#!/bin/bash

dir=$(dirname $0)

pushd $dir &>/dev/null
scripts=$(git ls-files | grep -v ".*/.*" | grep -v "^\..*" | sort )
#                        no subdir          no gitignore

popd  &>/dev/null

for f in $scripts
do
    ln -s $dir/$f
    echo $f
done
