#!/bin/sh

#export IFORTLIB="/opt/intel/lib:/opt/intel/mkl/lib" 
#export DYLD_LIBRARY_PATH="${IFORTLIB}"

# Estimating channel parameters (depth, width) by empirical equations of annual discharge
# Please execute this shell script in $CaMa-Flood/map/$MAPNAME directory
# % cd $MAPNAME
# % ../s01-channel_params.sh

################
# input runoff forcings

cd ..

TYPE='bin'          # plain binary (Fortran direct access)
INTERP='inpmat'     # runoff interpolation using input matrix

DIMINFO='./diminfo_test-1deg.txt'                 # dimention info to specity CaMa-Flood and Input Data resolutions

CROFBIN="../data/ELSE_GPCC_clm-1981-2010.one"     # long term mean runoff [mm/s]

## calculate annual discharge
echo ""
echo "@@@ calc_outclm $TYPE $INTERP $DIMINFO $CROFBIN"

./src_param/calc_outclm $TYPE $INTERP $DIMINFO $CROFBIN


######## below for old version (365 day climatology)
####CROFBIN='../data/runoff_1981-2000_day.bin'   # Climatology of Daily Runoff (365 day records in mm/day)
####CROFBIN="../data/ELSE_GPCC_coastmod_dayclm-1981-2010.one"
####./src_param/calc_outclmday $TYPE $INTERP $DIMINFO $CROFBIN


###  for netCDF runoff climatology  ###
# TYPE='cdf'
# CROFCDF='runoff.nc'  # netCDF runoff climatrogy file
# CROFVAR='RO'         # netCDF runoff variable name

# ../calc_outclm $TYPE $INTERP $DIMINFO $CROFCDF $CROFVAR

##############################################
# Channel bathymetry parameters

HC=0.1                                      ## HC:Coefficient, HP:Power, and HMIN:Minimum for channel depth
HP=0.50                                      #  H=max( HMIN, HC*Qave**HP+HO )
HO=0.00     ## height offset
HMIN=1.0

WC=2.50                                      ## WC:Coefficient, WP:Power, and WMIN:Minimum for channel width
WP=0.60                                      #  W=max( WMIN, WC*Qave**WP+WO )
WO=0.00
WMIN=5.0


## calculate channel parameters 
##   output width: rivwth.bin, depth: rivhgt.bin

echo "" 
echo "@@@ calc_rivwth $TYPE $DIMINFO $HC $HP $HMIN $WC $WP $WMIN"

./src_param/calc_rivwth $TYPE $DIMINFO $HC $HP $HO $HMIN $WC $WP $WO $WMIN


##############################################
# Channel width from Global Width Database for Large Rivers (GWD-LR)
#   Width of small channel (<300m) is given from rivwth.bin

## set channel width from GWD-LR
##   output GWD-LR width: rivwth_gwdlr.bin

echo "" 
echo "@@@ set_gwdlr $DIMINFO"

./src_param/set_gwdlr $DIMINFO

########################################
echo "" 
echo "@@@ set_bifparam $DIMINFO"

BIFLAYER=5   ## number of bifurcation layers. 1: Only channel bifurcation. max=10
             ##  (Faster simulation with small number, Accurate floodplain bifurcation with larger number)

./src_param/set_bifparam rivhgt.bin bifprm.txt $BIFLAYER $DIMINFO 


########################################
# Calculate parmanent water mask
#   Total area of partmanent water in each unit-catchment, using water map data.

TAG="1min"          # tag for hires data dir (1min / 30sec / 15sec / 3sec)
if [ -f ./1min/location.txt ]; then
  TAG="1min"
elif [ -f ./15sec/location.txt ]; then
  TAG="15sec"
fi

echo "" 
echo "@@@ calc_prmwat $TAG"

./src_param/calc_prmwat $TAG
