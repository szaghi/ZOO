#!/usr/bin/env bash

# Update ZOO by means of fresh git clone of GitHub repositories

ZAGHI=git@github.com:szaghi

rm -rf src
mkdir -p downloads

git clone $ZAGHI/PENF.git downloads/PENF
mkdir -p src/PENF
mv downloads/PENF/src/lib/* src/PENF/

git clone $ZAGHI/BeFoR64.git downloads/BeFoR64
mkdir -p src/BeFoR64
mv downloads/BeFoR64/src/lib/* src/BeFoR64/

rm -rf downloads
