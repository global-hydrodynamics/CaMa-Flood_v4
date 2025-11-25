#!/bin/sh

## CaMa-Flood: simulation map directory & simulation output dorectory
MAPDIR="./map/"                           # map directory
INPTIF="./data/FLOPROS_shp_V1_30s_filled.tif"    # FLOPROS protection standard (30sec raster)
OUTTIF="${MAPDIR}/protect.tif"            # Output Geotif file (resolution and domain converted to CaMa-Flood map)
OUTBIN="${MAPDIR}/protect.bin"            # Output Plain Binary file
PARAMS="${MAPDIR}/params.txt"             # Map parameter file

# Calculate Levee Unprotected Fraction from River-Levee Distance
# - output
#   map/protect.bin
#   map/protect.tif
python ./src/conv_FLOPROS_resolution.py $INPTIF $OUTTIF $OUTBIN $PARAMS

