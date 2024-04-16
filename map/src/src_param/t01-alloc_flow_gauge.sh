#!/bin/sh
# Allocate River Flow Gaunge to CaMa-Flood river network
########################################
# Sample allocating GRDC river flow gauges 

GAUGEINP='../../data/GRDC_allocated.csv'               # Gauge List File
GAUGEOUT='../GRDC_alloc.txt'

./allocate_flow_gauge $GAUGEINP multi
mv ./gauge_alloc.txt $GAUGEOUT
