#!/bin/sh

#### initial setting ========================================
## project name
TAG=glb_15min
OUT_DAMPARAM="./${TAG}/damparam_${TAG}.csv"

ND=`cat $TAG/dam_params_comp.csv | wc -l`
NDAMS=$(( $ND - 1 ))
echo "NDAMS allocated: $NDAMS"

echo "@@@ create dam param: $OUT_DAMPARAM"
echo "${NDAMS},NDAMS"           >  $OUT_DAMPARAM
cat ./$TAG/dam_params_comp.csv  >> $OUT_DAMPARAM

exit 0
