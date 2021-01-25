#!/bin/bash

USER=`whoami`

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4

#SYEAR=$5
#SMON=$6
#EYEAR=$7
#EMON=$8

RES=$5
NGRID=$6
MAXDPH=$7
OUTDIR=$8
FFLOOD=$9
RP=${10}
FUNC=${11}

rm -rf fig
mkdir -p fig

rm -f slp.bin
./src/conv_slp $WEST $EAST $SOUTH $NORTH $RES $NGRID    ## convert high-resolution slope file for PyThon figure.
cp slp.bin ${OUTDIR}/downscaled_flddph/.

./src/get_ele $WEST $EAST $SOUTH $NORTH $RES $NGRID    ##  obtain the high-resolution elevation file for PyThon figure.
cp ele.bin ${OUTDIR}/downscaled_flddph/.

./u02-flddph.sh $WEST $EAST $SOUTH $NORTH $NGRID $MAXDPH $RES $FFLOOD $RP $FUNC ##  draw figure

mv fig/* ${OUTDIR}/floodrisk_figure/.
rm -rf fig



#IYEAR=$SYEAR
#while [ $IYEAR -le $EYEAR ];
#do
#  IMON=1
#  while [ $IMON -le 12 ];
#  do
#    NDAY=`./src/igetday $IYEAR $IMON`   ## get number of days in a month
#    IDAY=1

#    while [ $IDAY -le $NDAY ];
#    do
#      CDATE=$(( $IYEAR * 10000 + $IMON * 100 + $IDAY ))
#      if [ $CDATE -lt $SDATE ]; then
#        echo "skip: $CDATE < $SDATE"
#      elif [ $CDATE -gt $EDATE ]; then
#        echo "skip: $CDATE > $EDATE"
#      else
#        echo "===== $CDATE ====="
#        ./u02-flddph.sh $WEST $EAST $SOUTH $NORTH $CDATE $NGRID $MAXDPH $RES    ##  draw figure
#
#        ## for parallel computation
#        NUM=`ps -U $USER | grep u02-flddph.sh | wc -l | awk '{print $1}'`
#        while [ $NUM -gt 8 ];
#        do
#          sleep 2
#          NUM=`ps -U $USER | grep u02-flddph.sh | wc -l | awk '{print $1}'`
#        done
#      fi
#    #IDAY=$(( $IDAY + 1 ))
#    IDAY=$(( $IDAY + $DINT ))
#    done
#  IMON=$(( $IMON + 1 ))
#  done
#IYEAR=$(( $IYEAR + 1 ))
#done

#wait
rm ./slp.bin
