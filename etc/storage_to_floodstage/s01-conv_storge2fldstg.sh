#!/bin/sh
#==========================================================
# CaMa-Flood script to convert storage to flood stage (post-process diagnose)
# (C) D.Yamazaki (U-Tokyo)  Apr 2025
#
# Licensed under the Apache License, Version 2.0 (the "License");
#   You may not use this file except in compliance with the License.
#   You may obtain a copy of the License at: http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is 
#  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and limitations under the License.
#================================================
# Basic setting

# (0) Set CaMa-Flood base directory
BASE=`pwd`/../../
# BASE="/home/yamadai/work/CaMa_v423/cmf_v423_pkg"  # setting for PBS in cluster
echo $BASE

#============================
# (1) Setting for conversion

#*** river map & topography used for simulation
FMAP="${BASE}/map/glb_15min"                # map directory
CPARAMS="${FMAP}/params.txt"                # dimention information file

CNEXTXY="${FMAP}/nextxy.bin"                # downstream xy (river network map)
CGRAREA="${FMAP}/ctmare.bin"                # unit-catchment area   [m2]
CELEVTN="${FMAP}/elevtn.bin"                # channel top elevation [m]
CRIVLEN="${FMAP}/rivlen.bin"                # channel length        [m]
CFLDHGT="${FMAP}/fldhgt.bin"                # floodplain elevation profile (height above 'elevtn') [m]

CRIVWTH="${FMAP}/rivwth_gwdlr.bin"          # channel width [m] (GWD-LR + filled with empirical)
CRIVHGT="${FMAP}/rivhgt.bin"                # channel depth [m] (empirical power-low)

#*** Setting for no levee simulation
#LLEVEE=".FALSE."                           # False for no-levee simulation
#
#NREC=10                                    # Number of data record to convert
#CSTORGE="${BASE}/out/test1-glb_15min/storge2000.bin"  # name of storage file
#
#COUTDIR='./diag_nolev/'                     # Direcory to save converted data
#CVARSOUT='rivsto,rivdph,fldsto,flddph,fldfrc,fldare,sfcelv'   # comma separated list of output variables

#*** Setting for with levee simulation
LLEVEE=".TRUE."                             # True for with levee simulation
CLEVFRC="${FMAP}/lev_frc_global.bin"                # levee unprotected fraction [0-1]
CLEVHGT="${FMAP}/lev_hgt_global_vic_up4_new.bin"    # levee height [m]

NREC=10                                     # Number of data record to convert
CSTORGE="${BASE}/out/levee_sample/storge2000.bin"  # name of storage file

COUTDIR='./diag_levee/'                     # Direcory to save converted data
CVARSOUT='rivsto,rivdph,fldsto,flddph,fldfrc,fldare,sfcelv,levsto,levdph'   # comma separated list of output variables



#================================================
# (2) Create NAMELIST for simulation year
# it is OK to remove optional variables (see conversion code)

NMLIST="./input.nam"           # Namelist for conversion Fortran90 code
rm -f ${NMLIST}

cat >> ${NMLIST} << EOF
&NMAP
CPARAMS    = "${CPARAMS}"              ! map dimention parameter
CNEXTXY    = "${CNEXTXY}"              ! river network nextxy
CGRAREA    = "${CGRAREA}"              ! catchment area
CELEVTN    = "${CELEVTN}"              ! bank top elevation
CRIVLEN    = "${CRIVLEN}"              ! river channel length
CFLDHGT    = "${CFLDHGT}"              ! floodplain elevation profile
CRIVWTH    = "${CRIVWTH}"              ! channel width
CRIVHGT    = "${CRIVHGT}"              ! channel depth

LLEVEE     = ${LLEVEE}
CLEVFRC    = "${CLEVFRC}"              ! levee unprotected fraction [0-1]
CLEVHGT    = "${CLEVHGT}"              ! levee height [m]

CSTORGE    = "${CSTORGE}"              ! simulated water storage
NREC       = ${NREC}                   ! record number to calculate flood stage

COUTDIR     = "${COUTDIR}"              ! directory to save output
CVARSOUT    = "${CVARSOUT}"            ! Comma-separated variables to output
/
EOF

echo "input.nam created:"
cat $NMLIST

#================================================
# (3) End of each year loop. Back to (3)

echo "execute conversion code: ./src/conv_storge2fldstg"
echo ""
echo ""
./src/conv_storge2fldstg

exit 0



