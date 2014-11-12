#!/bin/bash

cat *.summary | ./length.lisp 1800 > length.data
./length.gnuplot
