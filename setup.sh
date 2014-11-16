#!/bin/bash

install (){
    ../$1/install.sh > $1.list
    cat .gitignore $1.list | sort -u > tmpignore
    mv -f tmpignore .gitignore
    rm $1.list
}

install doubling
install aaai15-config

cp ~/repos/lisp/pddl.component-planner/aaai/component-planner ./
git submodule init
git submodule update

