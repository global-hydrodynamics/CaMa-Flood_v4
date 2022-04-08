#!/bin/sh

#### initial setting ========================================
## project name
TAG=glb_15min

OUT_DAMPARAM="./${TAG}/damparam_${TAG}.csv"

## minimum drainage area [km2]
MINUPAREA=1000

## Naturalized simulation (natsim) parameters
#Sample test1: test1-glb_15min
SYEAR=2000   ## Start year of natsim
EYEAR=2001   ## End   year of natsim
DT=86400     ## time step of outflw.bin data (sec)

#Sample test4: test4-e2o_ecmwf-glb_15min
#SYEAR=1980   ## Start year of natsim
#EYEAR=2014   ## End   year of natsim
#DT=86400     ## time step of outflw.bin data (sec)

#===========================================================

mkdir -p ./${TAG}

echo "============================"
echo "DAM allocation project: $TAG"
echo "============================"

echo ""
echo "#####  Part 1: allocate GRanD on CaMa map"

# ./t01-calc_damloc.sh   $TAG $MINUPAREA

# output dam allocation file : $TAG/damloc_modified.csv

##==========================
echo ""
echo "#####  Part 2: add dam paramerters"

# ./t02-calc_damparam.sh $TAG $MINUPAREA $SYEAR $EYEAR $DT

##==========================
echo ""
echo "#####  Part 3: merge dam location and parameter files"

echo "@@@ ./src/complete_damcsv.py"
#python ./src/complete_damcsv.py $TAG $MINUPAREA

# output $TAG/dam_params_comp.csv 

ND=`cat $TAG/dam_params_comp.csv | wc -l`
NDAMS=$(( $ND - 1 ))
echo "NDAMS allocated: $NDAMS"

echo "@@@ create dam param: $OUT_DAMPARAM"
echo "${NDAMS},NDAMS"           >  $OUT_DAMPARAM
cat ./$TAG/dam_params_comp.csv  >> $OUT_DAMPARAM

exit 0
