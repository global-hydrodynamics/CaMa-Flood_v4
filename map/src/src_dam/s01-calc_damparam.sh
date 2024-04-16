#!/bin/sh

#### initial setting ========================================
## project name
TAG=tmpdat
OUT_DAMPARAM="../dam_param.csv"

## Naturalized simulation (natsim) parameters
#Sample test1: test1-glb_15min (this is just for testing. better to use >30 year data)
SYEAR=2000   ## Start year of natsim
EYEAR=2001   ## End   year of natsim
DT=86400     ## time step of outflw.bin data (sec)

#Sample test4: test4-e2o_ecmwf-glb_15min
#SYEAR=1980   ## Start year of natsim
#EYEAR=2014   ## End   year of natsim
#DT=86400     ## time step of outflw.bin data (sec)

## minimum drainage area [km2] to exclude small reservoirs in Step (d)
MINUPAREA=0
#MINUPAREA=1000


#===========================================================
mkdir -p ./${TAG}

##==========================
# estimate paramters from naturalized simulation and reservoir datasets

##### (a) calculate mean and annual max discharge from naturalized simulations
echo "(a)----------"
echo "@@@ ./script/p01_get_annualmax_mean.py $SYEAR $EYEAR $DT $TAG"
echo "output (binary): tmp_p01_AnnualMax.bin, tmp_p01_AnnualMean.bin"
python ./script/p01_get_annualmax_mean.py $SYEAR $EYEAR $DT $TAG

###
##### (b) calculate 100year discharge from annual max discharge
echo "(b)----------"
echo "@@@ ./script/p02_get_100yrDischarge.py $SYEAR $EYEAR $TAG"
echo "output (binary): tmp_p02_100year.bin"
python ./script/p02_get_100yrDischarge.py $SYEAR $EYEAR $TAG

###
##### (c) estimate dam storage parameter, using GRSAD and ReGeom data
echo "(c)----------"
echo "@@@ ./script/p03_est_fldsto_surfacearea.py"
echo "output (csv): tmp_p03_fldsto.csv"
python ./script/p03_est_fldsto_surfacearea.py $TAG

## (d) merge dam location and parameter files"
echo "(d)----------"
echo "@@@ ./script/p04_complete_damcsv.py"
echo "output (csv): tmp_p04_damparams.csv"
python ./script/p04_complete_damcsv.py $TAG $MINUPAREA

##-----
## (e) Finalize parameter file"

ND=`cat $TAG/tmp_p04_damparams.csv | wc -l`
NDAMS=$(( $ND - 1 ))
echo "(e)----------"
echo "NDAMS allocated: $NDAMS"

echo "@@@ create dam param file:      $OUT_DAMPARAM"
echo "${NDAMS},NDAMS"              >  $OUT_DAMPARAM
cat ./$TAG/tmp_p04_damparams.csv    >> $OUT_DAMPARAM


echo ""
echo "###### NOTE ######"
echo "@@@ If needed, please manually edit the dam parameter file: " $OUT_DAMPARAM

exit 0
