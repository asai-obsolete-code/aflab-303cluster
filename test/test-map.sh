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

# sudo
read -p "enter sudo pass: " passwd
echodo ../map -s $passwd whoami
echodo ../map -s $passwd -- whoami
echodo ../map -s $passwd -- -- whoami

# symlink'd map script (default hosts)
echodo ./map ls

# symlink'd map script (hosts/hosts, fun000 only)
echodo hosts/map ls

# symlink'd map script (alternative host)
echodo ./map -h hosts2 ls

