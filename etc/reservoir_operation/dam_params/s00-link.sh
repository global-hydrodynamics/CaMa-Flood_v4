#!/bin/sh
# ===========================================================
# Link input data in inp/ dir
# ===========================================================

PWDD=`pwd`
BASE=`pwd`/../../../

## CaMa-Flood map directory to allocate dams
MAPDIR=${BASE}/map/glb_15min

## Dam input list: list of dams to be allocated
## Please prepare DAMLIST file (Dam allocated on the CaMa-Flood river map)
## GRanD reservoir allocation is done in map/src_param/alloc_dam.F90
DAMLIST=${MAPDIR}/GRanD_river.txt     ## full list

## NAT simulation: output directory 
## Execute simulation without dam to calculate mean and flood discharge, using same map data
## Output should be plain binary, and outflwYYYY.bin should be saved
NATDIR=${BASE}/out/test1-glb_15min
#NATDIR=${BASE}/out/test4-e2o_ecmwf-glb_15min

## GRSAD data (Global Reservoir Surface Area Dataset)
## Data can be downloaded below. Please specify the directory where "*_intp" files are located.
## https://dataverse.tdl.org/dataset.xhtml?persistentId=doi:10.18738/T8/DF80WG
#GRSADdir="/home/yamadai/work/data/Dam+Lake/GRSAD/GRSAD_timeseries/"
GRSADdir="/Users/yamadai/work/data/Dam+Lake/GRSAD/GRSAD_timeseries/"

## ReGeom data (Global Reservoir Geometry Database　＋Error File)
## Data can be downloaded below. Please specify the directory where "*.csv" data are located.
## https://zenodo.org/record/1322884#.YF1owUj7QW_
#ReGeomdir="/home/yamadai/work/data/Dam+Lake/ReGeom/Area_Strg_Dp/Area_Strg_Dp/"
ReGeomdir="/Users/yamadai/work/data/Dam+Lake/ReGeom/Area_Strg_Dp/"

#===========================================================


mkdir -p inp
cd ./inp

ln -fs $MAPDIR  ./map

ln -fs $DAMLIST ./damlist.txt

ln -fs $NATDIR  ./natsim

ln -fs $GRSADdir ./GRSAD

ln -fs $ReGeomdir ./ReGeom

exit 0
