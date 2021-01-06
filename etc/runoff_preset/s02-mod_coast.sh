#!/bin/sh

MAPDIR="../../map/glb_15min"
ROFFDIR="../../inp/ELSE_GPCC/Roff/"
ROFFPRE="Roff____"
ROFFSUF=".one"

DIMINFO="./map/diminfo_1deg.txt"
INPMAT="./map/inpmat-1deg.bin"

ln -sf $ROFFDIR runoff_ori
ln -sf $MAPDIR  map

ROFFAVE="./ELSE_GPCC_aveclm-1981-2010.one"  ## specify long-term averge runoff

SYEAR=1979
EYEAR=2010

# =======================

rm -rf   runoff_mod
mkdir -p runoff_mod

IYEAR=$SYEAR
while [ $IYEAR -le $EYEAR ];
do
  IMON=1
  while [ $IMON -le 12 ];
  do
    IDAY=1
    NDAY=`./src/igetday $IYEAR $IMON`
    while [ $IDAY -le $NDAY ];
    do
      CDATE=$(( IYEAR * 10000 + $IMON * 100 + $IDAY ))
      echo $CDATE
  
      ROFF="./runoff_ori/Roff____${CDATE}.one"
      FOUT="./runoff_mod/Roff____${CDATE}.one"
  
      if [ -f $ROFF ]; then
        echo $ROFF
        ./src/mod_runoff $ROFF $FOUT $DIMINFO $INPMAT $ROFFAVE
      fi
      IDAY=$(( IDAY + 1 ))
    done
    IMON=$(( $IMON + 1 ))
  done
  IYEAR=$(( $IYEAR + 1 ))
done


