#!/bin/sh
#==========================================================
# link map and simulation data (not mandatory, and not directly used in the scripts)

MAP="../../map/glb_15min"              # map data for simulation
OUT_NOLEV="../../out/test1-glb_15min"  # sample simulation without levee
OUT_LEVEE="../../out/levee_sample"     # sample simulation with    levee

ln -s $MAP map
ln -s $OUT_NOLEV out_nolev
ln -s $OUT_LEVEE out_levee


