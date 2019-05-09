#!/usr/bin/env bash

src=(
    "Basics.v"
    "Induction.v"
    "Lists.v"
    "Poly.v"
    "Tactics.v"
    "Logic.v"
    "IndProp.v"
    "Maps.v"
    "ProofObjects.v"
)

for i in "${src[@]}";do
    coqc -Q LF/ LF LF/"$i"
done
