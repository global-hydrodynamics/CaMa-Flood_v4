#!/bin/sh
#==========================================================
# CaMa-Flood sample map script-2: generate input matrix
#
# (C) D.Yamazaki & E. Dutra  (U-Tokyo/FCUL)  Aug 2019
#
# Licensed under the Apache License, Version 2.0 (the "License");
#   You may not use this file except in compliance with the License.
#   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is 
#  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and limitations under the License.
#==========================================================

# Please execute this shell script in $CaMa-Flood/map/$MAPNAME directory
# % cd $MAPNAME
# % ../s02-generate_inpmat.sh

cd ..

# Specify filename, input gridsize, input domain edge, input north-south order, diminfo & inpmat name

DIMINFO="diminfo_test-15min_nc.txt"
INPMAT="inpmat_test-15min_nc.bin"
#--------------------
# parameter for sample "test_15min_nc" runoff data
GRSIZEIN=0.25       # input grid size
WESTIN=-180.0       # input domain west east north south edge
EASTIN=180.0
NORTHIN=90.0
SOUTHIN=-60.0
OLAT="NtoS"         # north-south order of input data (if south to north, use StoN)
#OLAT="StoN"
#--------------------

#+++++++++++++++++++++++++++++++++++++++++++
### below is the example for sample test3-japan simulation in gosh
#DIMINFO="diminfo_test-tej.txt"
#INPMAT="inpmat_test-tej.bin"
##--------------------
## parameter for sample "test_15min_nc" runoff data
#GRSIZEIN=0.0166666666666666666666       # input grid size
#WESTIN=123.0       # input domain west east north south edge
#EASTIN=148.0
#NORTHIN=46.0
#SOUTHIN=24.0
#OLAT="NtoS"        # north-south order of input data
#+++++++++++++++++++++++++++++++++++++++++++


TAG="1min"          # tag for hires data dir (1min / 30sec / 15sec / 3sec)
if [ -f ./1min/location.txt ]; then
  TAG="1min"
elif [ -f ./15sec/location.txt ]; then
  TAG="15sec"
fi

#######################
# generate inpmat
#   Fortran code is for linear Cartesian input
#   for other grid coordinate, code should be rewritten

./src_param/generate_inpmat $TAG $GRSIZEIN $WESTIN $EASTIN $NORTHIN $SOUTHIN $OLAT $DIMINFO $INPMAT



#######################
# NOTE: INPUT GRID AND CaMa-Flood MAP ARE DIFFERENT GRIDS
#
# The CaMa-Flood map defines the river network and unit-catchment structure.
# The runoff input grid defines where the supplied runoff data are located.
# Their resolution, extent, and dimensions do not have to be the same.
# The map grid is read separately from this map directory's params.txt.
#
# generate_inpmat uses the map's high-resolution catchment lookup (selected by
# TAG below): it assigns each high-resolution pixel center to a runoff-grid
# cell, then accumulates that pixel's area for the corresponding CaMa
# catchment and runoff cell. This produces area-based map-to-runoff weights;
# it is not a requirement that the two grids align or have equal dimensions.
# Pixels outside the configured runoff-grid extent are skipped.
#
# The settings below describe the RUNOFF INPUT GRID, not the CaMa-Flood map:
#   GRSIZEIN  regular input-grid spacing in degrees.
#   WESTIN/EASTIN and SOUTHIN/NORTHIN give its longitude/latitude limits in
#             degrees. The code calculates nxin and nyin by rounding the
#             limit span divided by GRSIZEIN. Set limits that span an integer
#             number of grid cells and match the runoff data metadata.
#             The first cell center is half a grid spacing east of WESTIN
#             and half a spacing south of NORTHIN.
#   OLAT      order of runoff rows: NtoS (north row first) or StoN (south row
#             first). The Fortran code reverses the row index for StoN.
# Longitude values may use a -180..180 or 0..360 convention, but choose
# limits consistent with the runoff data. The implementation shifts each map
# pixel longitude by at most one full turn to fit the configured limits.
#
# WARNING: Do not copy the CaMa-Flood map boundaries or resolution into these
# settings unless those values also describe the runoff dataset. If the input
# grid does not cover the map area you need, map pixels outside it are omitted.
# Check the runoff file's coordinate arrays/metadata, cell spacing, dimensions,
# longitude convention, and latitude row order before generating the matrix.
#
# Example only (regular 0.1-degree runoff grid with 0..360 longitudes):
# GRSIZEIN=0.1
# WESTIN=-0.05; EASTIN=359.95
# NORTHIN=90.05; SOUTHIN=-90.05
# OLAT="NtoS"
# These limits imply 3600 columns and 1801 rows. They are an example input
# grid and should not be used unless they match the runoff dataset.
#
# TAG below selects the map's high-resolution catchment data (location.txt and
# catmxy files); it does not select or describe the runoff grid. DIMINFO and
# INPMAT name the generated dimension metadata and input-matrix files. DIMINFO
# records input dimensions and map bounds, but not the runoff bounds, spacing,
# or row order; keep those settings with the runoff-grid metadata.
#######################



