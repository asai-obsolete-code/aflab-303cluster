#!/bin/bash

. ../utilities.sh

echodo ../map ls

# failure case in dce7d5b
echodo ../map ls -la

echodo ../map -- ls -la

echodo ../map -p -- ls -la

echodo ../map -l -- ls -la

echodo ../map --lucy -- ls -la

# ssh option
echodo ../map --lucy -- -V -- ls -la
