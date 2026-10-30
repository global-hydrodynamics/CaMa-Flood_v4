#!/bin/sh

# To regionalize glb_15min to mekong_15min (sample mekong map)
SOURCE="../../glb_15min/"  #        !! global map directory
WEST="90.0"              #         !! west edge (regional)
EAST="110.0"               #          !! east edge
SOUTH="5.0"               #          !! south edge
NORTH="35.0"               #          !! north edge

# To regionalize glb_06min to CONUS_06min (sample simulation test2)
#SOURCE="../../glb_06min/"  #        !! global map directory
#WEST="-130.0"              #         !! west edge (regional)
#EAST="-60.0"               #          !! east edge
#SOUTH="20.0"               #          !! south edge
#NORTH="55.0"               #          !! north edge

# To convert jpn_01min to TE-Japan domain (sample simulation test3)
#SOURCE="../../jpn_01min/"  #        !! global map directory
#WEST="123.0"               #         !! west edge (regional)
#EAST="148.0"               #          !! east edge
#SOUTH="24.0"               #          !! south edge
#NORTH="46.0"               #          !! north edge

# Save the source map and requested bounds for the regionalization programs.
# Output: region_info.txt (source path, west, east, south, north).
echo "$SOURCE"     >  region_info.txt
echo "$WEST"       >> region_info.txt
echo "$EAST"       >> region_info.txt
echo "$SOUTH"      >> region_info.txt
echo "$NORTH"      >> region_info.txt

# Please edit "region_info.txt" to make a regional map

# Crop the source map to the requested domain and write regional map files,
# ../params.txt, dim_change.txt, and nextxy_noedge.bin.
./cut_domain
# Keep only bifurcation pathways inside the regional domain.
# Output: ../bifori.txt.
./cut_bifway
# Derive regional river sequence, basin, and upstream/network lookup maps.
# Outputs include ../rivseq.bin, ../basin.bin, ../bsncol.bin, ../upgrid.bin,
# ../uparea_grid.bin, ../nxtdst_grid.bin, and ../rivlen_grid.bin.
./set_map

# Crop each listed high-resolution source dataset when its location.txt exists.
# Output: regional high-resolution layers and ../<tag>/location.txt.
HDIRS="1min 30sec 15sec 3sec"
for HIRES in $HDIRS
do
  echo  $SOURCE/$HIRES/location.txto
  if [ -f $SOURCE/$HIRES/location.txt ]; then
    mkdir -p             ../$HIRES
    ./combine_hires      $HIRES
  fi
done

# Build the parameterization input matrix used with sample annual-mean runoff
# in map/data to calculate channel width/depth parameters. This is not the
# simulation runoff input matrix; that is prepared later under src_param.
# Outputs: ../inpmat_test-1deg.bin and ../diminfo_test-1deg.txt.
if [ -f ../1min/location.txt ]; then
  ./generate_inpmat 1min
elif [ -f ../30sec/location.txt ]; then
  ./generate_inpmat 30sec
elif [ -f ../15sec/location.txt ]; then
  ./generate_inpmat 15sec
elif [ -f ../3sec/location.txt ]; then
  ./generate_inpmat 3sec
fi

# Write GrADS control files for the regional map binaries (metadata only).
./s02-wrte_ctl_map.sh

# Write GrADS control files for each available regional high-resolution map.
HIRES="1min 30sec 15sec 3sec"
for RES in $HIRES
do
  if [ -f ../${RES}/location.txt ]; then
    ./s03-wrte_ctl_hires.sh ${RES}
  fi
done

