#!/bin/bash

. summarize-util.sh

sd=$(dirname $(readlink -ef $0))

run (){
    main $(lambda -- "[[ \$length != '' ]]") dispatch
}

dispatch (){
    case $solver in
        ff2*)
            parproblem-ff >> $sd/$solver.summary 
            ;;
        fffd*)
            parproblem-fd >> $sd/$solver.summary 
            ;;
        fd2*)
            parproblem-fd >> $sd/$solver.summary 
            ;;
        cea2*)
            parproblem-fd >> $sd/$solver.summary 
            ;;
        mv2)
            parproblem-mv >> $sd/$solver.summary
            ;;
        *)
            parproblem-std >> $sd/$solver.summary
            ;;
    esac
}

find -name "*summary" -delete
run
