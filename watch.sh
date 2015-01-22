#!/bin/bash

watch -d -t $(dirname $(readlink -ef $0))/status.sh
