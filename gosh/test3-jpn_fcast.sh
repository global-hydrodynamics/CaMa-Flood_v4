#!/bin/sh
#==========================================================
# CaMa-Flood sample go script (3) Japan forecast-like simulation
# -- Modified map for Japan 1min (tej_01min), based on jpn_01min
# -- Hourly runoff forcing (plain binary) at 1min resolution
# -- Forecast-like simulation: 39hour simulation from 6am/6pm every day
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

#*** PBS setting when needed
#PBS -q F20
#PBS -l select=1:ncpus=20:mem=10gb
#PBS -j oe
#PBS -m ea
#PBS -V

#================================================
# (0) Basic Setting (for workstation)

#*** 0a. Set CaMa-Flood base directory
BASE=`pwd`/..
# BASE="/home/yamadai/work/CaMa_v410/cmf_v410_pkg"  # setting for PBS in cluster

echo $BASE

#*** 0b. Set dynamic library if needed
export IFORTLIB="/opt/intel/lib:/opt/intel/mkl/lib"
export HDF5LIB="/opt/local/hdf5-1.10.5/lib"
export DYLD_LIBRARY_PATH="${HDF5LIB}:${IFORTLIB}:${DYLD_LIBRARY_PATH}"

#*** 0c. OpenMP thread number
export OMP_NUM_THREADS=16                  # OpenMP cpu num
#export OMP_NUM_THREADS=20                  # OpenMP cpu num

#================================================
# (1) Experiment setting
# -- some non-default options can be modified in NAMELIST section 

#============================
#*** 1a. Experiment directory setting
EXP="test3-jpn_fcast"                       # experiment name (output directory name)
#RDIR=${BASE}/out/${EXP}                     # directory to run CaMa-Flood
EXE="MAIN_cmf"                              # Execute file name
PROG=${BASE}/src/${EXE}                     # location of Fortran main program
NMLIST="./input_cmf.nam"                    # standard namelist
LOGOUT="./log_CaMa.txt"                     # standard log output

#============================
#*** 1b. Model physics option
#DT=86400                                    # base DT (modified in physics loop by LADPSTP)
DT=3600                # hourly input       # base DT (modified in physics loop by LADPSTP)
LADPSTP=".TRUE."                            # .TRUE. for adaptive time step
LPTHOUT=".TRUE."                            # .TRUE. to activate bifurcation flow, mainly for delta simulation

#============================
#*** 1c. simulation time
YSTA=2007                                   # start year ( from YSTA / Jan  1st _ 00:00)
MSTA=9
DSTA=3
YEND=2007                                   # end   year (until YEND / Dec 31st _ 24:00)
MEND=9
DEND=8
#SPINUP=0                                    # [0]: zero-storage start, [1]: from restart file
#NSP=0                                       # spinup repeat time

#============================
#*** 1d. spinup setting

#* input restart file
LRESTART="" # see (3) set each year         # TRUE. to use restart initial condition
CRESTSTO="" # see (3) set each year         # input restart FIle

#* output restart file
CRESTDIR="./"                               # output restart file directory
CVNREST="restart"                           # output restart file prefix
LRESTCDF=".FALSE."                          # .TRUE. to use netCDF restart file
IFRQ_RST="3"                                # output restat frequency.
                                            # [0]: only at last time, [1,2,3,...,24] hourly restart, [30]: monthly restart
#============================
#*** 1e. forcing setting
#IFRQ_INP="24"                               # input forcing frequency: [1,2,3,...,24] hour
#DROFUNIT="86400000"   # [mm/day->m/s]       # runoff unit conversion
IFRQ_INP="1"                                # input forcing frequency: [1,2,3,...,24] hour
DROFUNIT="3600000"   # [mm/hr->m/s]         # runoff unit conversion

#----- for plain binary runoff forcing
LINPCDF=".FALSE."                           # true for netCDF runoff
LINTERP=".TRUE."                            # .TRUE. to interporlate with input matrix
CROFDIR="${BASE}/inp/test_jpn_1hr/"         # runoff directory
CROFPRE="runoff_"                           # runoff prefix/suffix  
CROFSUF=".bin"                              #   $(CROFPRE)YYYYMMDD$(CROFSUF)

#============================
#*** 1f. river map & topography
FMAP="${BASE}/map/tej_01min"                # map directory
CDIMINFO="${FMAP}/diminfo_test-tej.txt"     # dimention information file
CINPMAT=${FMAP}/inpmat_test-tej.bin         # runoff input matrix for interporlation
#CDIMINFO="${FMAP}/diminfo_test-15min_nc.txt" # dimention information file
#CINPMAT=${FMAP}/inpmat_test-15min_nc.bin     # runoff input matrix for interporlation

#----- for plain binary map input
#** basic topography
LMAPCDF=".FALSE."                           # .TRUE. for netCDF map
CNEXTXY="${FMAP}/nextxy.bin"                # downstream xy (river network map)
CGRAREA="${FMAP}/ctmare.bin"                # unit-catchment area   [m2]
CELEVTN="${FMAP}/elevtn.bin"                # channel top elevation [m]
CNXTDST="${FMAP}/nxtdst.bin"                # downstream distance   [m]
CRIVLEN="${FMAP}/rivlen.bin"                # channel length        [m]
CFLDHGT="${FMAP}/fldhgt.bin"                # floodplain elevation profile (height above 'elevtn') [m]

#** channel parameter
###CRIVWTH=${FMAP}/rivwth.bin"              # channel width [m] (empirical power-low)
CRIVWTH="${FMAP}/rivwth_gwdlr.bin"          # channel width [m] (GWD-LR + filled with empirical)
CRIVHGT="${FMAP}/rivhgt.bin"                # channel depth [m] (empirical power-low)
CRIVMAN="${FMAP}/rivman.bin"                # manning coefficient river (The one in flood plain is a global parameter; set $PMANFLD below.)

#** bifurcation channel info
CPTHOUT="${FMAP}/bifprm.txt"                #   bifurcation channel list

#============================
#*** 1h. Output Settings 
#IFRQ_OUT=24                                 # output frequency: [1,2,3,...,24] hour
IFRQ_OUT=1                                  # output frequency: [1,2,3,...,24] hour

LOUTCDF=".FALSE."                           # .TRUE. netCDF output, .FALSE. plain binary output
COUTDIR="./"                                # output directory 
CVARSOUT="outflw,storge,fldfrc,maxdph,flddph" # list output variable (comma separated)
#CVARSOUT="rivout,rivsto,rivdph,rivvel,fldout,fldsto,flddph,fldfrc,fldare,sfcelv,outflw,storge,pthflw,pthout,maxsto,maxflw,maxdph" # list output variable (comma separated)
COUTTAG=""  # see (3) set each year         #   output tag $(COUTDIR)/$(VARNAME)$(OUTTAG).bin

##### Model Parameters ################
PMANRIV="0.03D0"                            # manning coefficient river
PMANFLD="0.10D0"                            # manning coefficient floodplain
PCADP="0.7"                                 # satety coefficient for CFL condition
PDSTMTH="10000.D0"                          # downstream distance at river mouth [m]



#================================================
# (2) Initial setting

ENDDATE=$(( $MEND * 100   + $DEND ))
ENDDATE=$(( $YEND * 10000 + $ENDDATE ))

#*** 2a. create running dir 
mkdir -p ${BASE}/out/${EXP}

SYEAR=${YSTA}
SMON=${MSTA}
SDAY=${DSTA}
SHOUR=6

EYEAR=${SYEAR}
EMON=${SMON}
EDAY=$(( $SDAY + 1 ))     ## 39hr simulation
EHOUR=$(( $SHOUR + 15 ))
if [ $EHOUR -ge 24 ]; then
  EDAY=$(( $EDAY + 1 ))
  EHOUR=$(( $EHOUR - 24 ))
fi

SDATE=$(( $SMON  * 100   + $SDAY  ))
SDATE=$(( $SYEAR * 10000 + $SDATE ))
SCHH=`printf %02d ${SHOUR}`

EDATE=$(( $EMON  * 100   + $EDAY  ))
EDATE=$(( $EYEAR * 10000 + $EDATE ))
ECHH=`printf %02d ${EHOUR}`

RDIRPRE="NONE"

#================================================
# (3) For each simulation day, modify setting
#--  loop 48hour simulation for each day

while [ $EDATE -lt $ENDDATE ];
do
  CDATE="${SDATE}${SCHH}"
  RDIR=${BASE}/out/${EXP}/${CDATE}                     # directory to run CaMa-Flood
  COUTTAG=${CDATE}                  # output file tag

  mkdir -p $RDIR
  cd       $RDIR

  if [ -e ${RDIRPRE}/restart${CDATE}.bin ]; then
    cp -f ${RDIRPRE}/restart${CDATE}.bin      .
    cp -f ${RDIRPRE}/restart${CDATE}.bin.pth .
    LRESTART=".TRUE."
    CRESTSTO="./${CVNREST}${CDATE}.bin"    ## from restart file
  else
    LRESTART=".FALSE."                  ## from zero storage
    CRESTSTO="NONE"
  fi

  echo "$CDATE -> ${EDATE}${ECHH} : $RDIR $LRESTART $CRESTSTO"

  ln -sf $PROG $EXE

#================================================
# (4) Create NAMELIST for simulation year
# it is OK to remove optional variables (set to default in CaMa-Flood)

rm -f ${NMLIST}

#*** 0. config
cat >> ${NMLIST} << EOF
&NRUNVER
LADPSTP  = ${LADPSTP}                  ! true: use adaptive time step
LPTHOUT  = ${LPTHOUT}                  ! true: activate bifurcation scheme
LRESTART = ${LRESTART}                 ! true: initial condition from restart file
/
&NDIMTIME
CDIMINFO = "${CDIMINFO}"               ! text file for dimention information
DT       = ${DT}                       ! time step length (sec)
IFRQ_INP = ${IFRQ_INP}                 ! input forcing update frequency (hour)
/
&NPARAM
PMANRIV  = ${PMANRIV}                  ! manning coefficient river
PMANFLD  = ${PMANFLD}                  ! manning coefficient floodplain
PDSTMTH  = ${PDSTMTH}                  ! downstream distance at river mouth [m]
PCADP    = ${PCADP}                    ! CFL coefficient
/
EOF

#*** 1. time
cat >> ${NMLIST} << EOF
&NSIMTIME
SYEAR   = ${SYEAR}                     ! start year
SMON    = ${SMON}                      !  month 
SDAY    = ${SDAY}                      !  day 
SHOUR   = ${SHOUR}                     !  houe
EYEAR   = ${EYEAR}                     ! end year
EMON    = ${EMON}                      !  month 
EDAY    = ${EDAY}                      !  day 
EHOUR   = ${EHOUR}                     !  hour
/
EOF

#*** 2. map
cat >> ${NMLIST} << EOF
&NMAP
LMAPCDF    = ${LMAPCDF}                ! * true for netCDF map input
CNEXTXY    = "${CNEXTXY}"              ! river network nextxy
CGRAREA    = "${CGRAREA}"              ! catchment area
CELEVTN    = "${CELEVTN}"              ! bank top elevation
CNXTDST    = "${CNXTDST}"              ! distance to next outlet
CRIVLEN    = "${CRIVLEN}"              ! river channel length
CFLDHGT    = "${CFLDHGT}"              ! floodplain elevation profile
CRIVWTH    = "${CRIVWTH}"              ! channel width
CRIVHGT    = "${CRIVHGT}"              ! channel depth
CRIVMAN    = "${CRIVMAN}"              ! river manning coefficient
CPTHOUT    = "${CPTHOUT}"              ! bifurcation channel table
/
EOF

#*** 3. restart
cat >> ${NMLIST} << EOF
&NRESTART
CRESTSTO = "${CRESTSTO}"               ! restart file
CRESTDIR = "${CRESTDIR}"               ! restart directory
CVNREST  = "${CVNREST}"                ! restart variable name
LRESTCDF = ${LRESTCDF}                 ! * true for netCDF restart file (double precision)
IFRQ_RST = ${IFRQ_RST}                 ! restart write frequency (1-24: hour, 0:end of run)
/
EOF

#*** 4. forcing
cat >> ${NMLIST} << EOF
&NFORCE
LINPCDF  = ${LINPCDF}                  ! true for netCDF runoff
LINTERP  = ${LINTERP}                  ! true for runoff interpolation using input matrix
CINPMAT  = "${CINPMAT}"                ! input matrix file name
DROFUNIT = ${DROFUNIT}                 ! runoff unit conversion
CROFDIR  = "${CROFDIR}"                ! runoff             input directory
CROFPRE  = "${CROFPRE}"                ! runoff             input prefix
CROFSUF  = "${CROFSUF}"                ! runoff             input suffix
/
EOF

#*** 5. outputs
cat >> ${NMLIST} << EOF
&NOUTPUT
COUTDIR  = "${COUTDIR}"                ! OUTPUT DIRECTORY
CVARSOUT = "${CVARSOUT}"               ! Comma-separated list of output variables to save 
COUTTAG  = "${COUTTAG}"                ! Output Tag Name for each experiment
LOUTCDF  = ${LOUTCDF}                  ! * true for netcdf outptu false for binary
IFRQ_OUT = ${IFRQ_OUT}                 ! output data write frequency (hour)
/
EOF

#================================================
# (5) Execute main program

echo "start: ${CDATE}" `date`  >> log.txt
time ./${EXE}                  >> log.txt 
echo "end:   ${CDATE}" `date`  >> log.txt

mv ${LOGOUT} log_CaMa-${CDATE}.txt

#================================================
# (6) manage simulation step

RDIRPRE=$RDIR

SYEAR=${SYEAR}
SMON=${SMON}
SDAY=${SDAY}
SHOUR=$(( $SHOUR + 12 ))   ## advance 12hr
if [ $SHOUR -ge 24 ]; then
  SDAY=$(( $SDAY + 1 ))
  SHOUR=$(( $SHOUR - 24 ))
fi
NSDAY=`$BASE/util/igetday $SYEAR $SMON`
if [ $SDAY -gt $NSDAY ]; then
  SDAY=1
  SMON=$(( $SMON + 1 ))
fi
if [ $SMON -gt 12 ]; then
  SMON=1
  SYEAR=$(( $SYEAR +1 ))
fi

EYEAR=${SYEAR}
EMON=${SMON}
EDAY=$(( $SDAY + 1 ))     ## 39hr simulation
EHOUR=$(( $SHOUR + 15 ))
if [ $EHOUR -ge 24 ]; then
  EDAY=$(( $EDAY + 1 ))
  EHOUR=$(( $EHOUR - 24 ))
fi
NEDAY=`$BASE/util/igetday $EYEAR $EMON`
if [ $EDAY -gt $NEDAY ]; then
  EDAY=1
  EMON=$(( $EMON + 1 ))
fi
if [ $EMON -gt 12 ]; then
  EMON=1
  EYEAR=$(( $EYEAR +1 ))
fi

SDATE=$(( $SMON  * 100   + $SDAY  ))
SDATE=$(( $SYEAR * 10000 + $SDATE ))
SCHH=`printf %02d ${SHOUR}`

EDATE=$(( $EMON  * 100   + $EDAY  ))
EDATE=$(( $EYEAR * 10000 + $EDATE ))
ECHH=`printf %02d ${EHOUR}`


#================================================
# (7) End of each year loop. Back to (3)

done # loop to next year simulation

exit 0
