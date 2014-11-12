#!/bin/bash

# cat *.summary | ./costs.lisp 1800 > costs.data
# ./costs.gnuplot

# cat fd.summary fd2.summary | ./costs-indexsep.lisp 1800

scatter (){
    ./costs-scatter.gnuplot \
        3< <(cat $1.summary $2.summary | ./costs.lisp 1800) \
        > $1-$2.pdf
}

scatter-index (){
    for ((i=1;i<=12;i++)) ;
    do
        gnuplot -e "i=$i" \
            -e "set xlabel 'cost $1'" \
            -e "set ylabel 'cost CAP($2)" \
            ./costs-scatter-index.gnuplot \
            3< <(cat $1.summary $2.summary | ./costs-indexsep.lisp 1800) \
            > $1-$2-$i.pdf
    done
}

# scatter fd fd2
# scatter cea cea2
# scatter probe probe2
# scatter fd fffd

scatter-index fd fd2
scatter-index cea cea2
scatter-index probe probe2
scatter-index fd fffd

