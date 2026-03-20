#!/bin/sh

MAPDIR="./map"
NEXTXY_INP="nextxy.bin"
BASIN_INP="bifbsn.bin"   ## basin mask considering bifrucation channel connectivity
#BASIN_INP="./map/basin.bin"   ## basin mask without considering bifrucation channel connectivity

NEXTXY_OUT="nextxy_inflow.bin"  ## output nextxy
MASK_OUT="upst_mask.bin"   ## output basin file

INFLIST="./data/list_points.txt"  ## list of inflow points [ NAME, IX, IY, (UPAREA) ]

MODE="default"  ## "default" for only keeping basin with inflow points
#MODE="keep"     ## "keep" for keeping all basin regardless of inflow points

./src/mask_upbasin $MAPDIR $NEXTXY_INP $BASIN_INP $NEXTXY_OUT $MASK_OUT $INFLIST $MODE