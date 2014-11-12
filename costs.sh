#!/bin/bash

cat *.summary | ./costs.lisp 1800 > costs.data
