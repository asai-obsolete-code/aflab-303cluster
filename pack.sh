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


echo $(fn)

tar czf $(fn).tar.gz $(ls -d */)

mv -vf *.tar.gz ../results/
