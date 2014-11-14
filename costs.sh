#!/bin/bash

plot (){
    echo $*
    cat $1.summary $2.summary | ./costs.lisp 1800
}


plot fd fd2
plot fd ff2
plot fd fffd
plot ff fffd
plot ff ff2

plot cea cea2
plot probe probe2
plot mv mv2

