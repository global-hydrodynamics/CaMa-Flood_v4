#!/bin/sh

USER=`whoami`

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4

SYEAR=$5
SMON=$6
EYEAR=$7
EMON=$8

RES=$9

SDATE=$(( $SYEAR * 10000 + $SMON * 100 + 1 ))
EDATE=$(( $EYEAR * 10000 + $EMON * 100 + 31 ))

rm -rf flood
mkdir -p flood

IYEAR=$SYEAR
while [ $IYEAR -le $EYEAR ];
do

  IMON=1
  IREC=0
  while [ $IMON -le 12 ];
  do
    NDAY=`./src/igetday $IYEAR $IMON`  ## get number of days in a month
    IDAY=1
    while [ $IDAY -le $NDAY ];
    do
      IREC=$(( $IREC + 1 ))   ## Record number of target date in the original flood depth file

      CDATE=$(( $IYEAR * 10000 + $IMON * 100 + $IDAY))
      if [ $CDATE -lt $SDATE ]; then
        echo "skip: $CDATE < $SDATE"
      elif [ $CDATE -gt $EDATE ]; then
        echo "skip: $CDATE > $EDATE"
      else
        FLDDPH="./out/flddph${IYEAR}.bin"      # input:  original flood depth file
        FFLOOD="./flood/flood_${CDATE}.bin"    # output: downscaled flood depth file

        ./src/downscale_flddph $WEST $EAST $SOUTH $NORTH $RES $FLDDPH $FFLOOD $IREC &

        ## for parallel computation using multiple CPUs 
        NUM=`ps aux | grep $USER | grep src/downscale_flddph | wc -l | awk '{print $1}'`
        while [ $NUM -gt 8 ];
        do
          sleep 1
          NUM=`ps aux | grep $USER | grep src/downscale_flddph | wc -l | awk '{print $1}'`
        done
      fi
    IDAY=$(( $IDAY + 1 ))
    done
  IMON=$(( $IMON + 1 ))
  done

IYEAR=$(( $IYEAR + 1 ))
done

wait