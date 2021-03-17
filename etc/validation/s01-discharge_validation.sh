 #!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
## Below is the example to prepare graphs for the validation purposes 
## for the result of sample simulation "test2"
## src/discharge_validation.py : discharge validation

CAMADIR="../../"

# ##### For test2 simulation #####
# MAPDIR="../../map/conus_06min"
# OUTDIR="../../out/test2-conus_06min"
# OBSDIR="./obs_sample/discharge"
# LIST="./obs_sample/discharge/discharge_list_conus_06min.txt"  ## Discharge location list [as a text file]
# ## validation project tag
# TAG="conus"
# #select the output file type [netcdf/bin]
# OUTPUT="netcdf"
#################################

##### For test1 simulation #####
MAPDIR="../../map/glb_15min"
# OUTDIR="../../out/test1-glb_15min"
OUTDIR="../../out/test-dev_HanazakiDam"
# OBSDIR="../../obs_sample/discharge"
OBSDIR="/cluster/data6/menaka/GRDC_2019"
# LIST="../../obs_sample/discharge/discharge_list_glb_15min.txt"  ## Discharge location list [as a text file]
LIST=${MAPDIR}"/GRDC_alloc.txt"  ## Discharge location list [as a text file]
#### validation project tag
TAG="glb_Dam"
###select the output file type [netcdf/bin]
OUTPUT="bin"
# OUTPUT="netcdf"
#################################

echo "MAPDIR, OUTDIR, OBSDIR= " $MAPDIR, $OUTDIR, $OBSDIR


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

rm -f  list.txt
ln -sf $LIST  list.txt

mkdir -p fig/discharge
mkdir -p txt/discharge
##########

# make validation figures for discharge
echo "### DISCHARGE VISUALIZATION"
python src/discharge_validation.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $OUTPUT

##########

## figures
rm -rf   fig_${TAG}/discharge
mkdir -p fig_${TAG}
mv       fig/discharge    fig_${TAG}/discharge
rm -rf   fig
echo "\n### figures saved in directory: fig_${TAG}/discharge"

## validation data
rm -rf   txt_${TAG}/discharge
mkdir -p txt_${TAG}
mv       txt/discharge    txt_${TAG}/discharge
rm -rf   txt
echo "\n### validation data saved in directory: txt_${TAG}/discharge"
