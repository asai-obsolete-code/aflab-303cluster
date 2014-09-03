#!/bin/bash

dir=$(dirname $0)

pushd $dir
scripts=$(git ls-files | grep -v ".*/.*" | grep -v "^\.[^\]*")
popd

for f in $scripts
do
    ln -s $dir/$f
done
