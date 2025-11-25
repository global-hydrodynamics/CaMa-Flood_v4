#!/bin/sh

USER=`whoami`

WEST=$1
EAST=$2
SOUTH=$3
NORTH=$4

RES=3sec

FLDDPH="./missi.bin"      # input:  original flood depth file
FFLOOD="./flood/flood_${CDATE}.bin"    # output: downscaled flood depth file

        ./src/downscale_flddph_nolev $WEST $EAST $SOUTH $NORTH $RES $FLDDPH $FFLOOD $IREC 

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