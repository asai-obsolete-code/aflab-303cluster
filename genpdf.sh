#!/bin/bash

rm -v *.pdf

./preprocess.sh
./preprocess-per-domain.sh
./costs.sh

basedirname=$(basename $(pwd))
dir=~/Dropbox/FukunagaLabShare/repos/icaps15/$basedirname/

rm -rv $dir
mkdir -pv $dir
cp -fv *.pdf $dir
