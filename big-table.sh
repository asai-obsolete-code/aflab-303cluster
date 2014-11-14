#!/bin/bash
# start

cat *.summary | $(dirname $(readlink -ef $0))/big-table.lisp 1800 | tee ~/repos/papers/aaai15/big-table.tex
