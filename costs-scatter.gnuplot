#! /usr/bin/gnuplot

set terminal pdf

set output "$1"

set key autotitle columnhead
plot "<&1" using 4:5 w p

set output