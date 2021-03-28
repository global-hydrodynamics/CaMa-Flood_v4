#!/bin/sh
# ===========================================================
# Link input data in inp/ dir
# ===========================================================

PWDD=`pwd`
BASE=`pwd`/../../../

## Dam input list: list of dams to be allocated
## Please prepare DAMLIST file based on GRanD data
## columns should be [DAM_ID, DAMNAME, LON, LAT, CATCH_SKM (catchment area in km2), CAP_MCM (capacity in million m3)]
DAMLIST=${PWDD}/GRanD_sample/dam_inplist_sample.csv
#DAMLIST=/home/yamadai/work/data/Dam+Lake/GRanD/inp/GRanD_v1_1_inp.csv   ## full list

## CaMa-Flood map directory to allocate dams
MAPDIR=${BASE}/map/glb_15min

## NAT simulation: output directory 
## Execute simulation without dam to calculate mean and flood discharge, using same map data
## Output should be plain binary, and outflwYYYY.bin should be saved
NATDIR=${BASE}/out/test1-glb_15min
#NATDIR=${BASE}/out/test4-e2o_ecmwf-glb_15min

## GRSAD data (Global Reservoir Surface Area Dataset)
## https://dataverse.tdl.org/dataset.xhtml?persistentId=doi:10.18738/T8/DF80WG
GRSADdir="/home/yamadai/work/data/Dam+Lake/GRSAD/GRSAD_timeseries/"

## ReGeom data (Global Reservoir Geometry Database)
## https://zenodo.org/record/1322884#.YF1owUj7QW_
ReGeomdir="/home/yamadai/work/data/Dam+Lake/ReGeom/Area_Strg_Dp/Area_Strg_Dp/"
ReGeom_ErrorFile="/home/yamadai/work/data/Dam+Lake/ReGeom/ReGeomData_WOW_V1.csv"

#===========================================================


mkdir -p inp
cd ./inp

ln -fs $DAMLIST ./damlist.csv

ln -fs $MAPDIR  ./map

ln -fs $NATDIR  ./natsim

ln -fs $GRSADdir ./GRSAD

ln -fs $ReGeomdir ./ReGeom

ln -fs $ReGeom_ErrorFile ./ReGeom_Error.csv

exit 0
