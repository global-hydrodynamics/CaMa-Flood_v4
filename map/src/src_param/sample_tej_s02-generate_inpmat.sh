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

#######################
# Specify filename, input gridsize, input domain edge, input north-south order, diminfo & inpmat name

#DIMINFO="diminfo_test-15min_nc.txt"
#INPMAT="inpmat_test-15min_nc.bin"
##--------------------
## parameter for sample "test_15min_nc" runoff data
#GRSIZEIN=0.25       # input grid size
#WESTIN=-180.0       # input domain west east north south edge
#EASTIN=180.0
#NORTHIN=90.0
#SOUTHIN=-60.0
#OLAT="NtoS"         # north-south order of input data (if south to north, use StoN)
#OLAT="StoN"
#--------------------

#+++++++++++++++++++++++++++++++++++++++++++
### below is the example for sample test3-japan simulation in gosh
DIMINFO="diminfo_test-tej.txt"
INPMAT="inpmat_test-tej.bin"
##--------------------
## parameter for sample "test_15min_nc" runoff data
GRSIZEIN=0.0166666666666666666666       # input grid size
WESTIN=123.0       # input domain west east north south edge
EASTIN=148.0
NORTHIN=46.0
SOUTHIN=24.0
OLAT="NtoS"        # north-south order of input data
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




