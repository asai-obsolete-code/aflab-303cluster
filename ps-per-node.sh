#!/bin/bash

ps -u asai -o pid,ppid,pgid,%cpu,%mem,rss,vsz,etime,s,comm --sort=-%mem | head -n 5 | tail -n +2
