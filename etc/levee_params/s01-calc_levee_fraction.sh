#!/bin/bash
## CaMa-Flood: simulation map directory & levee data files

MAPDIR="../../map/glb_15min/"                 # map directory
HIGHRES="1min"                                # high-res file to analyze (default: global=1min, Japan=15sec)

LEVLIST="./data/levdis_Zhao2025_glb3sec.txt"  # levee list file (levee distance and water type at river pixels, on MERIT Hydro or J-FlwDir)
LISTRES="3sec"                                # resolution of LEVLIST data (default; global=3sec MERIT Hydro, Japan=1sec J-FlwDir)

RIVWTH="./map/rivwth_gwdlr.bin"                   # river width data

ln -sf   $MAPDIR map
mkdir -p tmp

# calculate levee fraction. 
# - output:
#     map/levfrc.bin        levee fraction
#     map/levhgt_sim00.bin    infinite levee height (for first guess)

./src/calc_levfrc $HIGHRES $LEVLIST $LISTRES $RIVWTH

