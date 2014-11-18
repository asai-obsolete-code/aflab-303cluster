#!/bin/bash
# start

# name=$(basename $(pwd))
#  | tee ~/repos/papers/aaai15/$name-table.tex

cat *.summary | $(dirname $(readlink -ef $0))/big-table.lisp $@
