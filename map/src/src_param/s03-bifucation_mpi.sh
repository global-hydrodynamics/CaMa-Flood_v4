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

########################################
echo "" 
echo "@@@ set_bifparam $DIMINFO"

DIMINFO='./diminfo_test-1deg.txt'                 # dimention info to specity CaMa-Flood and Input Data resolutions
BIFLAYER=5   ## number of bifurcation layers. 1: Only channel bifurcation. max=10
             ##  (Faster simulation with small number, Accurate floodplain bifurcation with larger number)

./src_param/set_bifparam rivhgt.bin tmp.txt $BIFLAYER $DIMINFO 

sort -n -k 15n -k 16n -k 6n tmp.txt  > bifprm.txt

cd src_param/

########################################
echo "" 
echo "@@@ set_bif_basin"

#######################################################'
#If you want to use more MPI nodes : please add argument MaxMPI'
#% ./set_bif_basin MaxMPI '
# '

#When MaxMPI is specified:'
# - a few inter-basin channels neglected, such as Amazon-Orinoco connection' 
# - most inter-basin bifurcations in delta regions are still represented' 
#'       
#Up to 16 MPI nodes available for usual case' 
#When MaxMPI is specified, up to 30 MPI nodes are available' 
#######################################################'
TAG=''
#TAG='MaxMPI'

./set_bif_basin $TAG

########################################
echo "" 
echo "@@@ set_mpi_region"

./set_mpi_region 4
./set_mpi_region 8
./set_mpi_region 16

cd ..
cp mpireg-16.bin mpireg.bin

