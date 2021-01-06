#!/bin/sh

SOURCE="../../glb_15min/"  #        !! global map directory
WEST="90.0"               #         !! west edge (regional)
EAST="110.0"               #          !! east edge
SOUTH="5.0"              #          !! south edge
NORTH="35.0"               #          !! north edge

echo "$SOURCE"     >  region_info.txt
echo "$WEST"       >> region_info.txt
echo "$EAST"       >> region_info.txt
echo "$SOUTH"      >> region_info.txt
echo "$NORTH"      >> region_info.txt

# Please edit "region_info.txt" to make a regional map

./cut_domain
./cut_bifway
./set_map

HDIRS="1min 15sec 3sec"
for HIRES in $HDIRS
do
  echo  $SOURCE/$HIRES/location.txto
  if [ -f $SOURCE/$HIRES/location.txt ]; then
    mkdir -p             ../$HIRES
    ./combine_hires      $HIRES
  fi
done

if [ -f ../1min/location.txt ]; then
  ./generate_inpmat 1min
elif [ -f ../15sec/location.txt ]; then
  ./generate_inpmat 15sec
elif [ -f ../3sec/location.txt ]; then
  ./generate_inpmat 3sec
fi

./s02-wrte_ctl_map.sh

HIRES="1min 15sec 3sec"
for RES in $HIRES
do
  if [ -f ../${RES}/location.txt ]; then
    ./s03-wrte_ctl_hires.sh ${RES}
  fi
done

