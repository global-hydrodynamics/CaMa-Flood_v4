#!/bin/sh

#### initial setting ========================================

## project name
TAG="sample_glb_15min"

## dam information file
## columns should be [DAM_ID, DAMNAME, LON, LAT, CATCH_SKM, CAP_MCM]
DAMFILE=/cluster/data5/hanazaki/cmf_0325/CaMa-Flood_v4-dev400_damout/etc/reservoir_operation/sample_data/dam_params_sample2.csv
#DAMFILE=/cluster/data5/hanazaki/to_others/dam_params/inp/GRanD_v1_1_inp.csv

## number of dams
#NDAMS=6862
NDAMS=2

## CaMa-Flood map directory to allocate dams
MAPDIR=/cluster/data5/hanazaki/CaMa-Flood_v396a_20200514/map/glb_15min/

## NAT simulation directory 
NATDIR=/cluster/data5/hanazaki/CaMa-Flood_v396a_20200514/out/nodam_glb_15min_ERA5LAND_2001_2019

## NAT simulation period
SYEAR=2001
EYEAR=2002

## NAT simulation output frequency
DT=10800

## minimum drainage area to allow [km2]
MINUPAREA=1000

## GRSAD data
GRSADdir=/cluster/data5/hanazaki/data/GRSAD/GRSAD_timeseries/

## ReGeom data
ReGeomdir=/cluster/data5/hanazaki/data/ReGeom/Area_Strg_Dp/Area_Strg_Dp/
ReGeom_ErrorFile=/cluster/data5/hanazaki/data/ReGeom/ReGeomData_WOW_V1.csv

#===========================================================

./s00-link.sh $DAMFILE $MAPDIR $NATDIR $GRSADdir $ReGeomdir $ReGeom_ErrorFile

./s01-calc_damloc.sh $TAG $NDAMS $MINUPAREA

./s02-mk_damparams.sh $TAG $SYEAR $EYEAR $DT $MINUPAREA

echo "########################################"

exit 0
