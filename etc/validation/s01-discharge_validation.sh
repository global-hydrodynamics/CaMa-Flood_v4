#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test2"
## src/discharge_validation.py : discharge validation


MAPDIR="../../map/conus_06min"
OUTDIR="../../out/test2-conus_06min"
# OBSDIR="./obs_sample_git"
OBSDIR="../../obs/discharge"
CAMADIR="../../"

## Discharge location list [as a text file]
# LIST="../../obs/discharge/discharge_list_glb_15min.txt"
LIST="../../obs/discharge/discharge_list_conus_06min.txt"

echo "MAPDIR, OUTDIR, OBSDIR= " $MAPDIR, $OUTDIR, $OBSDIR

## validation project tag
TAG="conus"
# TAG="glb"

## specify validation period
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

rm -f list.txt
ln -sf $LIST  list.txt

mkdir -p fig/discharge
mkdir -p txt/discharge
##########

# select the output file type [netcdf/bin]
OUTPUT="netcdf"
# OUTPUT="bin"

# make validation figures for discharge
echo "### DISCHARGE VISUALIZATION"
python src/discharge_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $OUTPUT

##########

## figures
rm -rf fig_${TAG}/discharge
mkdir  fig_${TAG}
mv     fig/discharge    fig_${TAG}/discharge
rm -rf fig
echo "\n\n\n### figures saved in directory: fig_${TAG}/discharge"

## validation data
rm -rf txt_${TAG}/discharge
mkdir  txt_${TAG}
mv     txt/discharge    txt_${TAG}/discharge
rm -rf txt
echo "\n\n\n### validation data saved in directory: txt_${TAG}/discharge"