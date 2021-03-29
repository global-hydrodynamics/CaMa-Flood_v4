#!/bin/sh
#==========================================================
# Visualize the reservoir inflow/outflow for sample simulation

GRanD_obs="./sample_data/GRanD_reservoir"
#GRanD_obs="/home/yamadai/work/data/Dam+Lake/GRanD/GRanD_reservoir"

#### simulation 
MAPDIR="../../map/glb_15min"
DAMSIM="./out/test_dam"
NATSIM="./out/test_nat"
DAMLIST="./sample_data/damparam_sample_glb_15min.csv"
DT=10800   #output frequency

#### validation period
SYEAR=2000
SMON=1
SDAY=1
EYEAR=2001
EMON=12
EDAY=31

DAMIDLIST="601,421"   #comma separated

####

ln -sf $GRanD_obs ./obs_dam
ln -sf $MAPDIR map
ln -sf $DAMSIM damsim
ln -sf $NATSIM natsim
ln -sf $DAMLIST damlist.csv

python hydrograph_dam.py $SYEAR $SMON $SDAY $EYEAR $EMON $EDAY $DAMIDLIST $DT

exit 0