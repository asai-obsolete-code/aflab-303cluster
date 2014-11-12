#! /usr/bin/gnuplot

set terminal pdf

set output
# set logscale xy

set key autotitle columnhead

plot x, "<&3" index i using 4:5 w p


