#!/bin/sh

BASE=`pwd`
VER="v401"

PKG="cmf_${VER}_pkg"
SRC="cmf_${VER}_src"
DAT="cmf_${VER}_data"

####
echo "First, Please edir ${SRC}/adm/Mkinclude before executing this file"
echo "Making package starts in 3 seconds..."
sleep 2
###

# copy src to pkg
rm -rf $PKG
cp -r  $SRC $PKG

# copy sample input
cd ${BASE}/${PKG}/
cp -r ${BASE}/${DAT}/inp            ${BASE}/${PKG}/

# copy or link map data
cd ${BASE}/${PKG}/map
cp -r ${BASE}/${DAT}/map/data       ${BASE}/${PKG}/map/

cp -r ${BASE}/${DAT}/map/glb_15min  ${BASE}/${PKG}/map/
ln -s ${BASE}/${DAT}/map/glb_06min  ${BASE}/${PKG}/map/
ln -s ${BASE}/${DAT}/map/jpn_01min  ${BASE}/${PKG}/map/

# prepare sample regional map for test similations
cd ${BASE}/${PKG}/map
cp -r src/src_param glb_15min/
## prepare map parameters
  cd ${BASE}/${PKG}/map/glb_15min/src_param/
  make all
  ./s01-channel_params.sh
  ./s02-generate_inpmat.sh
  make clean

cd ${BASE}/${PKG}/map
mkdir -p conus_06min
cp -r src/src_region conus_06min/
cp -r src/src_param  conus_06min/
## prepare regional map
  cd ${BASE}/${PKG}/map/conus_06min/src_region/
  make all
  ./s01-regional_map.sh
  make clean
## prepare map parameters
cd ${BASE}/${PKG}/map
  cd ${BASE}/${PKG}/map/conus_06min/src_param/
  make all
  ./s01-channel_params.sh
  ./s02-generate_inpmat.sh
  make clean

cd ${BASE}/${PKG}/map
mkdir -p tej_01min
cp -r src/src_param  tej_01min/
cp -r src/src_region tej_01min/
## prepare regional map
  cd ${BASE}/${PKG}/map/tej_01min/src_region/
  make all
  ./sample_tej_s01-regional_map.sh
  make clean
## prepare map parameters
cd ${BASE}/${PKG}/map
  cd ${BASE}/${PKG}/map/tej_01min/src_param/
  make all
  ./s01-channel_params.sh
  ./s02-generate_inpmat.sh
  make clean

# prepare sample data for etc/dir
cp -r ${BASE}/${DAT}/etc/validation/obs_sample ${BASE}/${PKG}/etc/validation/

######
echo "Copy complete"
echo "Prepare regionalized map, and then make package by archive_pkg.sh"