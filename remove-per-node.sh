#!/bin/bash

old=/tmp/oldtmp

echorun (){
    echo $*
    $@
}

main (){
    echo hi!
    nohup ./mvrm /tmp/lisptmp
    nohup ./mvrm /tmp/newtmp
    list=$(ls /tmp/doubling*)
    rm -rv $list
}

main
echo finished!
