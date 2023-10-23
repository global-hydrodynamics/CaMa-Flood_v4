#!/bin/sh

# Allocate River Flow Gaunge to CaMa-Flood river network
########################################
GAUGEINP='../../data/GRDC_alloc.csv'               # Gauge List File
GAUGEOUT='../gauge.txt'

./allocate_gauge $GAUGEINP

mv ./gauge_alloc.txt $GAUGEOUT



DAMINP='../../data/GRanD_alloc.csv'               # Gauge List File
DAMOUT1='../GRanD_river.txt' # correctly allocated
DAMOUT2='../GRanD_small.txt' # too small to allocate on river map (sub-grid dam)

./allocate_dam $DAMINP

mv ./dam_alloc_river.txt $DAMOUT1
mv ./dam_alloc_small.txt $DAMOUT2
