#!/bin/bash

./map "find /sys/fs/cgroup/ -regex ".*/asai/[0-9]*/[0-9]*" | xargs rmdir "
./map "find /sys/fs/cgroup/ -regex ".*/asai/[0-9]*" | xargs rmdir "
