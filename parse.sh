#!/bin/bash

INPUT=${2:-/dev/stdin}

CONV_ID=$(grep "$1" "$INPUT" | head -n 1 | awk '{print $1}')

grep $CONV_ID "$INPUT" | awk -F'\t' '{print "[" $2 "] " $3 ": " $4}'
