#!/bin/bash

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4

RES=$5
TAG=$6

NGRID=1

FFLOOD="./flood/fldprd${TAG}.bin"    # output: downscaled flood depth file
SLOPE="./slope/slp${TAG}.bin"

rm -f $SLOPE
./src/conv_slp_dur $WEST $EAST $SOUTH $NORTH $RES $NGRID $SLOPE    ## convert high-resolution slope file for PyThon figure.

python ./draw_duration.py  $WEST $EAST $SOUTH $NORTH $FFLOOD $SLOPE $NGRID $RES $TAG          ## draw figure using PyThon


wait
