#!/bin/sh

#### initial setting ========================================
## project name
TAG=sample_glb_15min

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

echo ""
echo "@@@ src/get_rivinfo_glb "
./src/get_rivinfo_glb
mv ./damloc_tmp.txt ./$TAG/

echo ""
echo "@@@ ./src/modify_damloc.py "
python ./src/modify_damloc.py $TAG $MINUPAREA

##==========================
echo ""
echo "#####  Part 2: add dam paramerters"
## calculate mean & 100yr flood discharge from naturalized simulations
echo ""
echo "@@@ ./src/get_annualmax_mean.py"
python ./src/get_annualmax_mean.py $SYEAR $EYEAR $DT $TAG

echo ""
echo "@@@ ./src/get_100yrDischarge.py"
python ./src/get_100yrDischarge.py $SYEAR $EYEAR $TAG

## estimate dam storage parameter, using GRSAD and ReGeom data
echo ""
echo "@@@ ./src/est_fldsto_surfacearea.py"
python ./src/est_fldsto_surfacearea.py $TAG

##==========================
echo ""
echo "#####  Part 3: merge dam location and parameter files"
echo ""
echo "@@@ ./src/complete_damcsv.py"
python ./src/complete_damcsv.py $TAG $MINUPAREA

echo "@@@ create dam param: $OUT_DAMPARAM"
sed -n 1p ./inp/damlist.csv     >  $OUT_DAMPARAM
cat ./$TAG/dam_params_comp.csv  >> $OUT_DAMPARAM

exit 0
