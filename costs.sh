#!/bin/bash

cat *.summary | ./costs.lisp 1800 > costs.data
./costs.gnuplot


cat fd.summary fd2.summary | ./costs.lisp 1800 | ./costs-scatter.gnuplot fd.pdf

