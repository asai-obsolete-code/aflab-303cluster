#!/bin/bash


./map --profile --lucy -- -q -- "echo -n \"* \" ; hostname ; ./map --lucy -- -q -- hostname"

