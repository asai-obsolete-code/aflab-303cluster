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
    ls -d */ | grep -v planner-scripts
}

echo $(fn)

tar czf $(fn).tar.gz $(targets)

mv -vf *.tar.gz ../results/
