#!/bin/sh
## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test1"
## src/discharge_validation.py : discharge validation
## src/wse_validation.py : water surface elevation validation
## src/flood_validation.py : flodd extent validation **[to be added soon]
MAPDIR="../../map/conus_06min"
OUTDIR="../../out/test2-conus_06min"
# MAPDIR="../../map/glb_15min"
# OUTDIR="../../out/test1-glb_15min"
# OBSDIR="./obs_sample_git"
OBSDIR="../../obs"
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

mkdir -p fig/discharge
mkdir -p txt/discharge
# mkdir -p fig/wse
##########

# select the output file type [netcdf/bin]
OUTPUT="netcdf"
# OUTPUT="bin"

# make validation figures for discharge
echo "### DISCHARGE VISUALIZATION"
python src/discharge_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $OUTPUT

# OUTPUT="bin"
# # make validation figures for wse
# echo "\n\n\n### Water Surface Elevation VISUALIZATION"
# python src/wse_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $CAMADIR $EGM $OUTPUT


# # make validation figures for flood extent
# echo "\n\n\n### Flood Extent VISUALIZATION"

##########

## figures
rm -rf fig_${TAG}/discharge
mv   -p  fig/discharge    fig_${TAG}/discharge
echo "\n\n\n### figures saved in directory: fig_${TAG}"

## validation data
rm -rf txt_${TAG}/discharge
mv     txt/discharge    txt_${TAG}/discharge
echo "\n\n\n### validation data saved in directory: txt_${TAG}"