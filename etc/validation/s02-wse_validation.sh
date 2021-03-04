#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/wse_validation.py : water surface elevation validation

CAMADIR="../../" 

##### For test1 simulation #####
# MAP and OUTPUT DIR
MAPDIR="../../map/glb_15min"
OUTDIR="../../out/test1-glb_15min"
# select the output file type [netcdf/bin]
OUTPUT="bin"
# OUTPUT="netcdf"

# Observation data dir
OBSDIR="./obs_sample/wse/"
## WSE location list [as a text file]
LIST="./obs_sample/wse/wse_list_glb_15min.txt"
## specify the Reference Geoid for WSE observations [EGM08 or EGM96]
EGM="EGM08"
# EGM="EGM96"

## validation project tag
TAG="glb"
#################################

echo "MAPDIR, OUTDIR, OBSDIR= " $MAPDIR, $OUTDIR, $OBSDIR

## specity validation period
SYEAR=2000
SMON=1
SDAY=1
EYEAR=2001
EMON=12
EDAY=31

##########

rm -f map
rm -f out
rm -f obs
ln -sf $MAPDIR map
ln -sf $OUTDIR out
ln -sf $OBSDIR obs

rm -f  list.txt
ln -sf $LIST  list.txt

mkdir -p fig/wse
mkdir -p txt/wse
##########

# make validation figures for wse
echo "\n ### Water Surface Elevation VISUALIZATION"
python src/wse_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $EGM $OUTPUT

##########

rm -rf   fig_${TAG}/wse
mkdir -p fig_${TAG}
mv       fig/wse    fig_${TAG}/wse
rm -rf   fig
echo "\n ### figures saved in directory: fig_${TAG}/wse"

## validation data
rm -rf   txt_${TAG}/wse
mkdir -p txt_${TAG}
mv       txt/wse    txt_${TAG}/wse
rm -rf   txt
echo "\n ### validation data saved in directory: txt_${TAG}/wse"
