#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to downscale the result of sample simulation "test1"
MAPDIR="../../map/glb_15min/"
OUTDIR="../../out/test1-glb_15min/"

## downscale project tag
TAG="mek"

## specify tartget downscale domain 
WEST=102
EAST=108
SOUTH=9
NORTH=15

## specity downscale period
SYEAR=2000
SMON=9
EYEAR=2000
EMON=9

## specify downscale resolution (high resolution file directory should exist in the used map directory)
RES=1min
#RES=30sec
#RES=15sec
#RES=5sec

## For visualization:  NGRID=pixels to aggregate,  MAXDPH:maximum water depth for colorbar
NGRID=1
MAXDPH=10.0

##########

rm -f map
rm -f out
ln -sf $MAPDIR map
ln -sf $OUTDIR out

##########

## downscale target domain using fortran code
./t01-downscale.sh   $WEST $EAST $SOUTH $NORTH $SYEAR $SMON $EYEAR $EMON $RES

## visualization using PyThon
./t02-draw_flddph.sh $WEST $EAST $SOUTH $NORTH $SYEAR $SMON $EYEAR $EMON $RES $NGRID $MAXDPH

##########

rm -rf flood_${TAG}
rm -rf fig_${TAG}
mv     flood       flood_${TAG}
mv     fig         fig_${TAG}
