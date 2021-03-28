#!/bin/sh
#set -ax

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example for the result of sample simulation "test1"
MAPDIR="../../map/glb_15min/"
OUTDIR="../../out/test1-glb_15min/"

## downscale project tag
PROJ="mek_day"

## specify tartget downscale domain 
WEST=102
EAST=108
SOUTH=9
NORTH=15

## specity downscale period
YEAR=2000

## specify downscale resolution (high resolution file directory should exist in the used map directory)
RES=1min
#RES=3sec
#RES=15sec

## specify range of depth-duration calculation [m]
MAXDPH=20.0   # max flood depth
INTDPH=0.05    # depth interval for depth-duration calculation
NREC=`echo $MAXDPH $INTDPH | awk '{print $1/$2+1}'`

##########

rm -f map
rm -f out
ln -sf $MAPDIR map
ln -sf $OUTDIR out

rm -rf dat
rm -rf flood
rm -rf fig

mkdir -p dat
mkdir -p flood
mkdir -p fig
mkdir -p slope

##############################
# Calculate depth-duration

FFLDDPH=./out/flddph${YEAR}.bin
FFLDDAY=./dat/fldprd${YEAR}.bin

# calc number of days
IDAY=`./src/igetday $YEAR 2`
NDAY=365
if [ $IDAY -eq 29 ]; then
  NDAY=366
fi

./src/calc_depth-duration $FFLDDPH $FFLDDAY $NDAY $MAXDPH $INTDPH

##############################
## downscale target domain using fortran code

TAG=$YEAR
./t11-downscale.sh $WEST $EAST $SOUTH $NORTH $RES $FFLDDAY $INTDPH $NREC $TAG

##############################
## visualization using PyThon
TAG=$YEAR
./t12-draw_duration.sh $WEST $EAST $SOUTH $NORTH $RES $TAG

##########

rm -rf slope
rm -rf flood_${PROJ}
rm -rf fig_${PROJ}
mv     flood       flood_${PROJ}
mv     fig         fig_${PROJ}


