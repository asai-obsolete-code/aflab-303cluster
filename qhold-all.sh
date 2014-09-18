#!/bin/bash

qhold $(qstat -a | grep batch | cut -d. -f 1)
