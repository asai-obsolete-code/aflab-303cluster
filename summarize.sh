#!/bin/bash

. summarize-util.sh

sd=$(pwd)

run (){
    main $(lambda -- "[[ \$length != '' ]]") dispatch
}

dispatch (){
    case $solver in
        ff2*) actioncost >> $sd/$solver.summary ;;
        fffd*) actioncost >> $sd/$solver.summary ;;
        fd2*) actioncost >> $sd/$solver.summary ;;
         cea2*) actioncost >> $sd/$solver.summary ;;
        mv2*) actioncost >> $sd/$solver.summary ;;
        probe2*) actioncost >> $sd/$solver.summary ;;
        *) parproblem-std >> $sd/$solver.summary ;;
    esac
}

dispatch-dbg (){
    case $solver in
        ff2*) actioncost | tee -a $sd/$solver.summary ;;
        fffd*) actioncost | tee -a $sd/$solver.summary ;;
        fd2*) actioncost | tee -a $sd/$solver.summary ;;
         cea2*) actioncost | tee -a $sd/$solver.summary ;;
        mv2*) actioncost | tee -a $sd/$solver.summary ;;
        probe2*) actioncost | tee -a $sd/$solver.summary ;;
        *) parproblem-std | tee -a $sd/$solver.summary ;;
    esac
}


find -name "*summary" -delete
run
