#!/bin/bash

# exit on failure
set -e

# move to location of this script
cd "$( dirname "${BASH_SOURCE[0]}" )"

# build all example executables
dune build @examples

# spit all example scads into things
mkdir -p "things"
cd "things"
for ex in ../_build/default/examples/*.exe; do
    eval "./$ex"
done
