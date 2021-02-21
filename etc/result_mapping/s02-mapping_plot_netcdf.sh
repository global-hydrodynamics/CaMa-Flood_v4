# A sample script to check the results by plot the map
# This file works for netCDF file 
# NOTE that, To read netCDF file, netCDF4 is required
# Please check the follow website to install netCDF4 before using this sample script. 
# https://unidata.github.io/netcdf4-python/

## CaMa-Flood: simulation map directory & simulation output dorectory
MAPDIR="../../map/glb_15min/"
OUTDIR="../../out/test2-conus_06min/"


# calculate the x,y information from the map parameters
XSIZE=$(head -n 1 "${MAPDIR}/params.txt" | awk '{print $1}') # xsize of input data
YSIZE=$(head -n 2 "${MAPDIR}/params.txt" | tail -n 1 | awk '{print $1}') # ysize of input data


##########
## plot the discharge map 

# If the output is in netcdf format
python plot_nc.py $OUTDIR/o_outflw2000.nc outflw

