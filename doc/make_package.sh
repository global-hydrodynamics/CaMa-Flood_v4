#!/bin/sh

BASE=`pwd`

PKG="cmf_v400_pkg"
SRC="cmf_v400_src"
DAT="cmf_v400_data"

# copy src to pkg
rm -rf $PKG
cp -r $SRC $PKG

# copy sample input
cd ${BASE}/${PKG}/
cp -r ${BASE}/${DAT}/inp .

# copy or link map data
cd ${BASE}/${PKG}/map
cp -r ${BASE}/${DAT}/dat_map/data .

cp -r ${BASE}/${DAT}/dat_map/glb_15min .
ln -s ${BASE}/${DAT}/dat_map/glb_06min .
ln -s ${BASE}/${DAT}/dat_map/jpn_01min .


# prepare sample regional map for test similations
cd ${BASE}/${PKG}/map

cp -r src/src_param glb_15min/

mkdir -p conus_06min
cp -r src/src_param  conus_06min/
cp -r src/src_region conus_06min/

mkdir -p tej_01min
cp -r src/src_param  tej_01min/
cp -r src/src_region tej_01min/
