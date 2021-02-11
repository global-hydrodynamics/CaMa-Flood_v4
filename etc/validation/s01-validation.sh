#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/discharge_validation.py : discharge validation
## src/wse_validation.py : water surface elevation validation
## src/flood_validation.py : flodd extent validation **[to be added soon]
MAPDIR="../../map/glb_15min/"
OUTDIR="../../out/test1-glb_15min/"
CAMADIR="../../" 

## validation project tag
TAG="glb"

## specity validation period
SYEAR=2000
SMON=1
SDAY=1
EYEAR=2001
EMON=12
EDAY=31

## specify the Reference Geoid for WSE observations [EGM08 or EGM96]
EGM="EGM08"
# EGM="EGM96"

##########

rm -f map
rm -f out
ln -sf $MAPDIR map
ln -sf $OUTDIR out

mkdir -p fig/discharge
mkdir -p fig/wse
##########

# make validation figures for discharge
python src/discharge_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $CAMADIR &

# make validation figures for wse
python src/wse_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $CAMADIR $EGM &

##########

wait

rm -rf fig_${TAG}
mv     fig         fig_${TAG}