#!/bin/bash

echocat (){
    echo -n "$($@)-"
}

fn (){
    echocat basename $(pwd)
    echocat basename $(git symbolic-ref HEAD)
    num=$(git --no-pager log -1 --pretty=oneline | head -c6)
    echocat echo $num
    date +"%m-%d-%Y"
}

list=$(mktemp)

trap "rm $list" EXIT

find /tmp \
    -regex '/tmp/fastdownward.*' -o \
    -regex '/tmp/component-planner.*' -o \
    -regex '/tmp/macroff.*' -o \
    -regex '/tmp/.*lisptmp.*' \
    -prune > $list

tar czvf $(fn)-tmp-$(hostname).tar.gz -T $list


