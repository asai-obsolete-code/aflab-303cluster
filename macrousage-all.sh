#!/bin/bash
echo $(dirname $(readlink -ef $0))
cat *.summary | $(dirname $(readlink -ef $0))/macrousage.lisp 1800 2> /dev/null
cat *.summary | $(dirname $(readlink -ef $0))/macrousage.lisp 3600 2> /dev/null

cp -vf ./macrousage*.png ~/repos/papers/aaai15/staticimg/
