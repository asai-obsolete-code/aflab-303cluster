#!/bin/bash

user=$1

ps -u $user -o pid,ppid,pgid,%cpu,%mem,rss,vsz,etime,s,comm --sort=-%mem | head -n 5 | tail -n +2
