#! /usr/bin/gnuplot

set terminal pdf

set output "costs.pdf"
# set xtics
# plot for [j=2:8] "data" using 0:j:xticlabels(1) w l

set key autotitle columnhead
plot for [j=5:11] "costs.data" using 0:(column(j)/$4) w lp ps 0.5

set output