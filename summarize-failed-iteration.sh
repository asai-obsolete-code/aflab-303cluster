#!/bin/bash

. summarize-util.sh

sd=$(pwd)

run (){
    # main $(lambda -- "[[ \$length != '' ]]") dispatch
    main true dispatch
}

# ext (){
#     if [[ $length != '' ]]
#     then
#         echo summary
#     else
#         echo failed
#     fi
# }

dispatch (){
    case $solver in
        ff2*) actioncost >> $sd/$solver.summary ;;
        cea2*) actioncost >> $sd/$solver.summary ;;
        probe2*) actioncost >> $sd/$solver.summary ;;
        # 
        fd*) parproblem-failed-iteration >> $sd/$solver.summary ;;
        fffd*) parproblem-failed-iteration >> $sd/$solver.summary ;;
        fd2*) parproblem-failed-iteration >> $sd/$solver.summary ;;
        # 
        *) parproblem-std >> $sd/$solver.summary ;;
    esac
}

rm -fv *.summary *.failed
run
