#! gnuplot

set terminal png

set output "costs.png"

plot "data" using 2 w l, \
     "data" using 3 w l, \
     "data" using 4 w l

set output