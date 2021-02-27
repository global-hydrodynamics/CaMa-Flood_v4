#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/wse_validation.py : water surface elevation validation


MAPDIR="../../map/glb_15min"
OUTDIR="../../out/test1-glb_15min"
# OBSDIR="./obs_sample_git"
OBSDIR="../../obs"
CAMADIR="../../" 

## WSE location list [as a text file]
LIST="../../obs/wse/wse_list_glb_15min.txt"

echo "MAPDIR, OUTDIR, OBSDIR= " $MAPDIR, $OUTDIR, $OBSDIR

## validation project tag
TAG="glb"
# TAG="conus"

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

rm -f list.txt
ln -sf $LIST  list.txt

mkdir -p fig/wse
mkdir -p txt/wse
##########

# select the output file type [netcdf/bin]
OUTPUT="bin"
# OUTPUT="netcdf"

# make validation figures for wse
echo "\n\n\n### Water Surface Elevation VISUALIZATION"
python src/wse_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $EGM $OUTPUT

##########

rm -rf fig_${TAG}/wse
mkdir  fig_${TAG}
mv     fig/wse    fig_${TAG}/wse
rm -rf fig
echo "\n\n\n### figures saved in directory: fig_${TAG}/wse"

## validation data
rm -rf txt_${TAG}/wse
mkdir  txt_${TAG}
mv     txt/wse    txt_${TAG}/wse
rm -rf txt
echo "\n\n\n### validation data saved in directory: txt_${TAG}/wse"