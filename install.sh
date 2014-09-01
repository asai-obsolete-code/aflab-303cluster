#!/bin/bash

ls -1 $(dirname $0) | while read line ; do ln -s $(dirname $0)/$line ; done
