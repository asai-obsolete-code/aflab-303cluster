#!/bin/bash

cat *.summary | $(dirname $(readlink -ef $0))/coverage.lisp
