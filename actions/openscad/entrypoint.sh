#!/bin/bash

set -eux
if [ -z $2 ]; then
    openscad $1 -o ${1%.*}.stl
else
    openscad $1 -o $2
fi
