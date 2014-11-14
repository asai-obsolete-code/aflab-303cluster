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

targets (){
    ls -1 -d */ | grep -v planner-scripts
    ls *.summary
}

name=${1:-$(fn)}
echo $name

tar czf $name.tar.gz $(targets)

cp -v  $name.tar.gz ~/Dropbox/component-planner/
mv -vb $name.tar.gz ../results/
