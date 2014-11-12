#!/bin/bash


plot (){
    cat $1.summary $2.summary | ./costs-gp.lisp 1800
}







plot fd fd2
plot cea cea2
plot probe probe2
plot fd fffd
