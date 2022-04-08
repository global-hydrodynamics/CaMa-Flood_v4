#!/bin/sh

BASE=`pwd`
VER="v406"

PKG="cmf_${VER}_pkg"
SRC="cmf_${VER}_src"
DAT="cmf_${VER}_data"

####
echo "First, Please edir ${SRC}/adm/Mkinclude before executing this file"
echo "-- Making package starts in 3 seconds --"
sleep 3
###

########## copy src to pkg ##########
rm -rf $PKG
cp -r  $SRC $PKG

########## copy sample input ##########
echo "copy sample input data"
cd ${BASE}/${PKG}/
pwd
mkdir -p inp
cp -r ${BASE}/${DAT}/inp/test_1deg      ${BASE}/${PKG}/inp/
cp -r ${BASE}/${DAT}/inp/test_15min_nc  ${BASE}/${PKG}/inp/
cp -r ${BASE}/${DAT}/inp/test_jpn_1hr   ${BASE}/${PKG}/inp/

########## copy or link map data ##########
echo "copy/link maps"
cd ${BASE}/${PKG}/map
pwd
cp -r ${BASE}/${DAT}/map/data       ${BASE}/${PKG}/map/

cp -r ${BASE}/${DAT}/map/glb_15min  ${BASE}/${PKG}/map/
ln -s ${BASE}/${DAT}/map/glb_06min  ${BASE}/${PKG}/map/
ln -s ${BASE}/${DAT}/map/jpn_01min  ${BASE}/${PKG}/map/

# prepare sample regional map for test similations
cd ${BASE}/${PKG}/map
cp -r src/src_param glb_15min/
## prepare map parameters
  echo "channel param for glb_15min"
  cd ${BASE}/${PKG}/map/glb_15min/src_param/
  pwd
  make all
  ./s01-channel_params.sh
  ./s02-generate_inpmat.sh
  make clean

cd ${BASE}/${PKG}/map
mkdir -p conus_06min
cp -r src/src_region conus_06min/
cp -r src/src_param  conus_06min/
## prepare regional map
  echo "regional map for conus_06min"
  cd ${BASE}/${PKG}/map/conus_06min/src_region/
  pwd
  make all
  ./s01-regional_map.sh
  make clean
## prepare map parameters
  echo "channel param for conus_06min"
  cd ${BASE}/${PKG}/map/conus_06min/src_param/
  pwd
  make all
  ./s01-channel_params.sh
  ./s02-generate_inpmat.sh
  make clean

cd ${BASE}/${PKG}/map
mkdir -p tej_01min
cp -r src/src_param  tej_01min/
cp -r src/src_region tej_01min/
## prepare regional map
  echo "regional map for tej_01min"
  cd ${BASE}/${PKG}/map/tej_01min/src_region/
  pwd
  make all
  ./sample_tej_s01-regional_map.sh
  make clean
## prepare map parameters
  echo "channel param for tej_01min"
  cd ${BASE}/${PKG}/map/tej_01min/src_param/
  pwd
  make all
  ./s01-channel_params.sh
  ./sample_tej_s02-generate_inpmat.sh
  make clean

########## prepare sample data for etc/dir ##########
echo "sample data for etc/ add on"
cd ${BASE}/${PKG}/etc
pwd
cp -r ${BASE}/${DAT}/etc/validation/obs_sample ${BASE}/${PKG}/etc/validation/

######
echo "Copy complete"
echo "Prepare regionalized map, and then make package by archive_pkg.sh"

