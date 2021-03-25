#!/bin/sh

#### initial setting ========================================

THISFILE=$0

TAG=$1

## dam information file
DAMFILE=./inp/damfile

## number of dams
NDAMS=$2

## CaMa-Flood map directory to allocate dams
MAPDIR=./inp/map

## minimum drainage area [km2]
MINUPAREA=$3

#===========================================================

echo " "
echo "########################"
echo $0

mkdir ../${TAG}

## output file
tmpfile=../${TAG}/damloc_tmp.txt
outfile=../${TAG}/damloc_modified_${TAG}.csv

./src/get_rivinfo_glb $NDAMS $tmpfile

python ./src/modify_damloc.py $tmpfile $outfile $MINUPAREA

exit 0
