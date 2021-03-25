#!/bin/sh

#### initial setting #########################################
THISFILE=$0

TAG=$1
DAMFILE=./inp/damfile
MAPDIR=./inp/map

NATDIR=./inp/natsim
SYEAR=$2
EYEAR=$3
DT=$4

MINUPAREA=$5

GRSADdir=./inp/GRSAD

ReGeomdir=./inp/ReGeom
ReGeom_ErrorFile=./inp/ReGeom_Error

############################################################

echo " "
echo "###############################"
echo $0

## dam location file

python ./src/get_annualmax_mean.py $SYEAR $EYEAR ${DT} $TAG

python ./src/get_100yrDischarge.py $SYEAR $EYEAR $TAG

python ./src/est_fldsto_surfacearea.py $TAG

python ./src/complete_damcsv.py $TAG $MINUPAREA

exit 0
