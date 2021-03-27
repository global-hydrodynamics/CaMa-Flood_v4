#!/bin/sh
#==========================================================
# Visualize the reservoir inflow/outflow for sample simulation

GRanD_obs="./sample_data/GRanD_reservoir"
#GRanD_obs="/home/yamadai/work/data/Dam+Lake/GRanD/GRanD_reservoir"

ln -sf $GRanD_obs ./obs_dam

python hydrograph_dam.py

exit 0
