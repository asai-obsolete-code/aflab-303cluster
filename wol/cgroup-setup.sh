#!/bin/bash

test -z $1 && echo "Usage: cgroup-setup username
-- it creates cgroup named asai which can be freely modified by user asai"

getent passwd $1 || { echo "no user $1 found" ; exit 1 ;}

cgcreate -a $1:$1 -t $1:$1 -g cpuacct,memory:/$1
