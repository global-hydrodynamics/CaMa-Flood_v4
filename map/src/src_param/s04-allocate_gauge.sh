#!/bin/sh

# Allocate River Flow Gaunge to CaMa-Flood river network
########################################
GAUGEINP='../../data/GRDC_alloc.txt'               # Gauge List File
GAUGEOUT='../gauge.txt'

./allocate_gauge $GAUGEINP

mv ./gauge_alloc.txt $GAUGEOUT

