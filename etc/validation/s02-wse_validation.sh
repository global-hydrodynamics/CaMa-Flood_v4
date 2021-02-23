#!/bin/sh
## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/discharge_validation.py : discharge validation
## src/wse_validation.py : water surface elevation validation
## src/flood_validation.py : flodd extent validation **[to be added soon]
MAPDIR="../../map/glb_15min"
OUTDIR="../../out/test1-glb_15min"
# OBSDIR="./obs_sample_git"
OBSDIR="../../obs"
CAMADIR="../../" 

echo "MAPDIR, OUTDIR, OBSDIR= " $MAPDIR, $OUTDIR, $OBSDIR

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

## specify the validation domain for flood inundation
WEST=102
EAST=108
SOUTH=9
NORTH=15

##########

rm -f map
rm -f out
rm -f obs
ln -sf $MAPDIR map
ln -sf $OUTDIR out
ln -sf $OBSDIR obs

mkdir -p fig/discharge
mkdir -p fig/wse
##########

# OUTPUT="netcdf"
# # make validation figures for discharge
# echo "### DISCHARGE VISUALIZATION"
# python src/discharge_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $OUTPUT

OUTPUT="bin"
# make validation figures for wse
echo "\n\n\n### Water Surface Elevation VISUALIZATION"
python src/wse_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $CAMADIR $EGM $OUTPUT


# make validation figures for flood extent
echo "\n\n\n### Flood Extent VISUALIZATION"

##########

rm -rf fig_${TAG}
mv     fig         fig_${TAG}
echo "\n\n\n### figures saved in directory: fig_${TAG}"
