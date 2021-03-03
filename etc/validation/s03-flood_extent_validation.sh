#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/flood_validation.py : flood water extent validation in monthly scale

CAMADIR="../../" 

##### For test1 simulation #####
# MAP and OUTPUT DIR
MAPDIR="../../map/glb_15min"
OUTDIR="../../out/test1-glb_15min"
OBSDIR="./obs_sample/fwe"
OBSDIR="../../obs_sample/fwe"

# select the output file type [netcdf/bin]
OUTPUT="bin"
# OUTPUT="netcdf"

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

## specify the validation domain for flood inundation
WEST=-72.0
EAST=-54.0
SOUTH=-8.0
NORTH=0.0

##########

rm -f map
rm -f out
rm -f obs
ln -sf $MAPDIR map
ln -sf $OUTDIR out
ln -sf $OBSDIR obs

mkdir -p fig/fwe
mkdir -p txt/fwe
##########

# make validation figures for flood extent
echo "\n ### Flood Extent VISUALIZATION"
python src/flood_extent_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $WEST $EAST $SOUTH $NORTH $OUTPUT

##########

rm -rf   fig_${TAG}/fwe
mkdir -p fig_${TAG}
mv       fig/fwe    fig_${TAG}/fwe
rm -rf   fig
echo "\n ### figures saved in directory: fig_${TAG}/fwe"

## validation data
rm -rf   txt_${TAG}/fwe
mkdir -p txt_${TAG}
mv       txt/fwe    txt_${TAG}/fwe
rm -rf   txt
echo "\n ### validation data saved in directory: txt_${TAG}/fwe"
