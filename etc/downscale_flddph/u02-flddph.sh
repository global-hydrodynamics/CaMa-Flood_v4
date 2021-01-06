#!/bin/sh

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4
CDATE=$5
NGRID=$6
MAXDPH=$7
RES=$8

rm -f dph${CDATE}.bin
./src/conv_flood  $WEST $EAST $SOUTH $NORTH $CDATE $NGRID $RES $MAXDPH   ## convert flood depth file for visualization format
python ./draw_flddph.py  $WEST $EAST $SOUTH $NORTH $CDATE $NGRID $RES $MAXDPH           ## draw figure using PyThon

rm -f dph${CDATE}.bin
