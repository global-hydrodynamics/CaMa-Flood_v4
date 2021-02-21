# A sample script to check the results by plot the map
# This file works for plain binary format
# 
## CaMa-Flood: simulation map directory & simulation output dorectory
MAPDIR="../../map/glb_15min/"
OUTDIR="../../out/test1-glb_15min/"


# calculate the x,y information from the map parameters
XSIZE=$(head -n 1 "${MAPDIR}/params.txt" | awk '{print $1}') # xsize of input data
YSIZE=$(head -n 2 "${MAPDIR}/params.txt" | tail -n 1 | awk '{print $1}') # ysize of input data


##########
## plot the discharge map 
# If the output is in plain binary format
python plot.py $OUTDIR/outflw2000.bin $YSIZE $XSIZE 


