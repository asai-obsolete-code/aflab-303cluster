#!/bin/bash

sudo cgcreate -a $(whoami):$(whoami) -t $(whoami):$(whoami) -g cpuacct,memory:$(whoami)

