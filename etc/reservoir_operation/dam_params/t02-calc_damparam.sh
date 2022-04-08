#!/bin/sh

#### initial setting ========================================
## project name
TAG=$1

## minimum drainage area [km2]
MINUPAREA=$2

## Naturalized simulation (natsim) parameters
SYEAR=$3   ## Start year of natsim
EYEAR=$4   ## End   year of natsim
DT=$5      ## time step of outflw.bin data (sec)

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

