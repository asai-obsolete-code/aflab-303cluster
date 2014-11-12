#! /usr/bin/gnuplot

set terminal pdf

set output
set xlabel "cost x"
set ylabel "cost CAP(x+x)"
set logscale xy

set key autotitle columnhead
plot x, for [i=1:12] "<&3" using 4:5 w p index i


