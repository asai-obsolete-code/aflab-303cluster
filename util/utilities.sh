#!/bin/bash

truedir (){
    echo $1
    $(dirname $(readlink -ef $1))
}

echovar (){
    echo -n $1 " "
    eval "echo \$$1"
}

echodo (){
    echo $*
    $@
}

