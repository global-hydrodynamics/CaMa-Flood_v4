#!/bin/sh
#set -ax

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4
RES=$5
FLDDAY=$6
INTDPH=$7
NREC=$8
TAG=$9

FFLOOD="./flood/fldprd${TAG}.bin"    # output: downscaled flood depth file

./src/downscale_fldprd $WEST $EAST $SOUTH $NORTH $RES $FLDDAY $FFLOOD $INTDPH $NREC

