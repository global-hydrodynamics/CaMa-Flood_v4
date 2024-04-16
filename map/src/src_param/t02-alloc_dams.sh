#!/bin/sh
# Allocate River Flow Gaunge to CaMa-Flood river network
########################################
# Sample allocating GRanD reservoirs 
DAMINP='../../data/GRanD_allocated.csv'               # Gauge List File

DAMOUT1='../GRanD_river.txt' # correctly allocated on river network
DAMOUT2='../GRanD_small.txt' # small dams not on the mainstem of the grid. (treat as sub-grid dam)

./allocate_dam $DAMINP

mv ./dam_alloc_river.txt $DAMOUT1
mv ./dam_alloc_small.txt $DAMOUT2
