#!/bin/bash
# start

name=$(basename $(pwd))

title=$1

if [[ $title == "" ]]
then
    echo "no title, substituting with the dirname $name" >&2
    title=$name
fi

cat *.summary | $(dirname $(readlink -ef $0))/big-table.lisp $title | tee ~/repos/papers/aaai15/$name-table.tex
