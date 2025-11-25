#!/bin/sh

# copy levee input data from map/data/
#  distance between river and levee list
#  FLOPROS 30sec protection level geiTiff

mkdir -p data
cp ../../map/data/levee/levdis_Zhao2025_glb3sec.txt   data/
cp ../../map/data/levee/FLOPROS_shp_V1_30s_filled.tif data/
