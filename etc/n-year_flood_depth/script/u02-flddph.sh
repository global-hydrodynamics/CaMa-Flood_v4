#!/bin/sh

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4
NGRID=$5
MAXDPH=$6
RES=$7
FFLOOD=$8
RP=$9
FUNC=${10}


#rm -f dph${CDATE}.bin
cp ${FFLOOD} flood.bin
./src/conv_flood  $WEST $EAST $SOUTH $NORTH $NGRID $RES $MAXDPH    ## convert flood depth file for visualization format

FDPH='dph.bin'
python ./draw_flddph.py  $WEST $EAST $SOUTH $NORTH $NGRID $RES $MAXDPH  $FDPH $RP $FUNC        ## draw figure using PyThon

rm -f flood.bin
rm -f dph.bin
