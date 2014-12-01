#!/bin/bash


for user in $(ypcat passwd | cut -d: -f1)
do
    target="/udon/$user"
    ./map -s 3sokuzenkai "mkdir -p $target; chown $user:$user $target"
done
