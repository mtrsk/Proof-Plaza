#!/usr/bin/env bash

SRC_DIR=QC

src=(
    "Introduction.v"
)

for i in "${src[@]}";do
    coqc -Q $(SRC_DIR)/ $(SRC_DIR) $(SRC_DIR)/"$i"
done
