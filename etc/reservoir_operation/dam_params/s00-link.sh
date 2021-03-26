#!/bin/sh

#### initial setting ========================================

THISFILE=$0

DAMFILE=$1

MAPDIR=$2

NATDIR=$3

GRSADdir=$4

ReGeomdir=$5
ReGeom_ErrorFile=$6

#===========================================================

echo $0

if [ ! -e ./inp ]; then
    mkdir ./inp
fi
cd ./inp

ln -s $DAMFILE ./damfile

ln -s $MAPDIR ./map

ln -s $NATDIR ./natsim

ln -s $GRSADdir ./GRSAD

ln -s $ReGeomdir ./ReGeom

ln -s $ReGeom_ErrorFile ./ReGeom_Error

exit 0
