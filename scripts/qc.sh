#!/usr/bin/env bash

src=(
    "Introduction.v"
)

for i in "${src[@]}";do
    coqc -Q QC/ QC QC/"$i"
done
