#!/bin/bash

cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 1800 2> /dev/null
cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 3600 2> /dev/null
