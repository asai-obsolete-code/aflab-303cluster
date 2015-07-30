#!/bin/bash

SCRDIR=$(readlink -ef $(dirname $0)/../)

PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    $SCRDIR/wol/shutdown.sh room502

