#!/bin/sh

TAG=$1
MAPDIR="../"

if [ "${TAG}" = "1min" ]; then
  CNUM=60
  MWIN=0
elif [ "${TAG}" = "30sec" ]; then
  CNUM=120
  MWIN=30
elif [ "${TAG}" = "15sec" ]; then
  CNUM=240
  MWIN=30
elif [ "${TAG}" = "5sec" ]; then
  CNUM=720
  MWIN=10
elif [ "${TAG}" = "3sec" ]; then
  CNUM=1200
  MWIN=10
elif [ "${TAG}" = "1sec" ]; then
  CNUM=3600
  MWIN=5
fi

WESN_G=`./gradsinfo`

####################################

if [ -f ./${MAPDIR}/${TAG}/${TAG}.catmxy.bin ]; then   ## when non-tiled data exist

  mkdir -p tmpctl
  cd tmpctl
  rm -f *.ctl

  echo ${TAG}  $CNUM $WESN_G

  ### hires ###
  ../wrte_ctl_hires ${TAG}.catmxy.bin  ${TAG}.catmxy.ctl  int2 xy  $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.catmzz.bin  ${TAG}.catmzz.ctl  int1 x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.flddif.bin  ${TAG}.flddif.ctl  real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.grdare.bin  ${TAG}.grdare.ctl  real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.elevtn.bin  ${TAG}.elevtn.ctl  real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.uparea.bin  ${TAG}.uparea.ctl  real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.rivwth.bin  ${TAG}.rivwth.ctl  real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.hand.bin    ${TAG}.hand.ctl    real x   $CNUM $WESN_G
  ../wrte_ctl_hires ${TAG}.visual.bin  ${TAG}.visual.ctl  int1 x   $CNUM $WESN_G

  if [ -f ../${MAPDIR}/${TAG}/${TAG}.flwdir.bin ]; then
    ../wrte_ctl_hires ${TAG}.flwdir.bin  ${TAG}.flwdir.ctl  int1 x   $CNUM $WESN_G
  fi
  if [ -f ../${MAPDIR}/${TAG}/${TAG}.downxy.bin ]; then
    ../wrte_ctl_hires ${TAG}.downxy.bin  ${TAG}.downxy.ctl  int2 xy  $CNUM $WESN_G
  fi

  mv *.ctl ../${MAPDIR}/${TAG}/

  cd ..
  rm -rf tmpctl
fi

if [ $MWIN -gt 0 ]; then

  mkdir -p tmpctl
  cd tmpctl
  rm -f *.ctl

  LAT=-90
  while [ $LAT -lt 90 ];
  do
    LON=-180
    while [ $LON -lt 180 ];
    do
      CNAME=`../set_name $LON $LAT`
      if [ -f ../${MAPDIR}/${TAG}/${CNAME}.catmxy.bin ]; then
        echo ../${MAPDIR}/${TAG}/${CNAME}.catmxy.bin
        LON2=$(( $LON + $MWIN ))
        LAT2=$(( $LAT + $MWIN ))

        echo $CNAME $CNUM $LON $LON2 $LAT $LAT2

        ../wrte_ctl_hires ${CNAME}.catmxy.bin  ${CNAME}.catmxy.ctl  int2 xy  $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.catmzz.bin  ${CNAME}.catmzz.ctl  int1 x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.flddif.bin  ${CNAME}.flddif.ctl  real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.grdare.bin  ${CNAME}.grdare.ctl  real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.elevtn.bin  ${CNAME}.elevtn.ctl  real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.uparea.bin  ${CNAME}.uparea.ctl  real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.rivwth.bin  ${CNAME}.rivwth.ctl  real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.hand.bin    ${CNAME}.hand.ctl    real x   $CNUM $LON $LON2 $LAT $LAT2
        ../wrte_ctl_hires ${CNAME}.visual.bin  ${CNAME}.visual.ctl  int1 x   $CNUM $LON $LON2 $LAT $LAT2

        if [ -f ../${MAPDIR}/${TAG}/${CNAME}.flwdir.bin ]; then
          ../wrte_ctl_hires ${CNAME}.flwdir.bin  ${CNAME}.flwdir.ctl  int1 x   $CNUM $LON $LON2 $LAT $LAT2
        fi
        if [ -f ../${MAPDIR}/${TAG}/${CNAME}.downxy.bin ]; then
          ../wrte_ctl_hires ${CNAME}.downxy.bin  ${CNAME}.downxy.ctl  int2 xy  $CNUM $LON $LON2 $LAT $LAT2
        fi


        mv *.ctl ../${MAPDIR}/${TAG}/

      fi
    LON=$(( $LON + $MWIN ))
    done
  LAT=$(( $LAT + $MWIN ))
  done

  cd ..
  rm -rf tmpctl
fi




