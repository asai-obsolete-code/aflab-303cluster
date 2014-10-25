#!/bin/bash
# start
cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 3600 2> /dev/null
cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 1800 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 900 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 450 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 225 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 112 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 56 2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 28  2> /dev/null
# cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp 14 2> /dev/null
