#!/bin/bash

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
NGRID=${10}
MAXDPH=${11}

SDATE=$(( $SYEAR * 10000 + $SMON * 100 + 1 ))
EDATE=$(( $EYEAR * 10000 + $EMON * 100 + 31 ))

rm -rf fig
mkdir -p fig

rm -f slp.bin
./src/conv_slp $WEST $EAST $SOUTH $NORTH $RES $NGRID    ## convert high-resolution slope file for PyThon figure.

IYEAR=$SYEAR
while [ $IYEAR -le $EYEAR ];
do
  IMON=1
  while [ $IMON -le 12 ];
  do
    NDAY=`./src/igetday $IYEAR $IMON`   ## get number of days in a month
    IDAY=1

    while [ $IDAY -le $NDAY ];
    do
      CDATE=$(( $IYEAR * 10000 + $IMON * 100 + $IDAY ))
      if [ $CDATE -lt $SDATE ]; then
        echo "skip: $CDATE < $SDATE"
      elif [ $CDATE -gt $EDATE ]; then
        echo "skip: $CDATE > $EDATE"
      else
        echo "===== $CDATE ====="
        ./u02-flddph.sh $WEST $EAST $SOUTH $NORTH $CDATE $NGRID $MAXDPH $RES    ##  draw figure

        ## for parallel computation
        NUM=`ps aux | grep $USER | grep u02-flddph.sh | wc -l | awk '{print $1}'`
        while [ $NUM -gt 8 ];
        do
          sleep 2
          NUM=`ps aux | grep $USER | grep u02-flddph.sh | wc -l | awk '{print $1}'`
        done
      fi
    IDAY=$(( $IDAY + 1 ))
    done
  IMON=$(( $IMON + 1 ))
  done
IYEAR=$(( $IYEAR + 1 ))
done

wait
rm ./slp.bin