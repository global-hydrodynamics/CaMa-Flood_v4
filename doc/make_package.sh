#!/bin/sh

BASE=`pwd`
VER="v401"

PKG="cmf_${VER}_pkg"
SRC="cmf_${VER}_src"
DAT="cmf_${VER}_data"

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

mkdir -p conus_06min
cp -r src/src_param  conus_06min/
cp -r src/src_region conus_06min/

mkdir -p tej_01min
cp -r src/src_param  tej_01min/
cp -r src/src_region tej_01min/

# prepare sample data for etc/dir
cp -r ${BASE}/${DAT}/etc/validation/obs_sample ${BASE}/${PKG}/etc/validation/

######
echo "Copy complete"
echo "Prepare regionalized map, and then make package by archive_pkg.sh"