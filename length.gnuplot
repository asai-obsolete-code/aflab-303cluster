#! /usr/bin/gnuplot

set terminal pdf

set output "length.pdf"
# set xtics
# plot for [j=2:8] "data" using 0:j:xticlabels(1) w l

set key autotitle columnhead
plot for [j=4:10] "length.data" using 0:j w l lw 2

set output