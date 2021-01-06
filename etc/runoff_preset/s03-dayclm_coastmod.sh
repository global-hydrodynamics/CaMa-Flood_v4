#!/bin/sh

MAPDIR="../../map/glb_15min"
ROFFDIR="./runoff_mod/"
ROFFPRE="Roff____"
ROFFSUF=".one"

DIMINFO="./map/diminfo_1deg.txt"
INPMAT="./map/inpmat-1deg.bin"

SYEAR=1981
EYEAR=2010

DAYCLMDIR="./dayclm_coastmod"

ROFFCLM="./ELSE_GPCC_coastmod_dayclm-${SYEAR}-${EYEAR}.one"
ROFFAVE="./ELSE_GPCC_coastmod_aveclm-${SYEAR}-${EYEAR}.one"

########

rm -rf   $DAYCLMDIR
mkdir -p $DAYCLMDIR

IMON=1
while [ $IMON -le 12 ];
do
  ./src/day_clim $DIMINFO runoff_mod $ROFFPRE $ROFFSUF $DAYCLMDIR $SYEAR $EYEAR $IMON &
  IMON=$(( $IMON + 1 ))
done
wait

#########

rm -f roff_dayclm.bin
rm -f roff_aveclm.bin

./src/ave_clim $DIMINFO $DAYCLMDIR $ROFFSUF

mv roff_dayclm.bin $ROFFCLM   ## daily climatology runoff [mm/day], 365 records
mv roff_aveclm.bin $ROFFAVE   ## long-term average runoff [mm/year]


