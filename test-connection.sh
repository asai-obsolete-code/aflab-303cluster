#!/bin/bash


./map -l -- -q -- "echo -n \"* \" ; hostname ; ./map -l -- -q -- hostname"
