#!/bin/bash


plot (){
    cat $1.summary $2.summary | ./costs-gp.lisp 1800 | gnuplot
}







plot fd fd2
plot fd ff2
plot fd fffd

plot cea cea2
plot probe probe2

