#!/bin/bash


cat *.summary | ./preprocess.lisp 1800 | gnuplot

