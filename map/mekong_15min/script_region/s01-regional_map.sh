#!/bin/sh

# Run this script from the map-local script_region directory. Fortran programs
# use relative paths, so keep this directory as the working directory.
# Configure the source map and requested region before running this workflow.
# SOURCE is relative to this script_region directory (or an absolute path).
SOURCE="../../glb_15min/"
WEST="90.0"
EAST="110.0"
SOUTH="5.0"
NORTH="35.0"

# Shared executables must be built explicitly from map/tools/code/ before use.
CODE_DIR="../../tools/code"
for PROGRAM in region_cut_domain region_cut_bifway region_set_map region_combine_hires region_generate_inpmat
do
  if [ ! -x "$CODE_DIR/$PROGRAM" ]; then
    echo "Missing shared executable: $CODE_DIR/$PROGRAM" >&2
    echo "Build regional tools first: make -C ../../tools/code region" >&2
    exit 1
  fi
done

# Save the source map and requested bounds for the regionalization programs.
# Output: region_info.txt (source path, west, east, south, north).
printf '%s\n' "$SOURCE" "$WEST" "$EAST" "$SOUTH" "$NORTH" > region_info.txt

# Crop the source map and write regional map files and dimension metadata.
"$CODE_DIR/region_cut_domain"
# Keep only bifurcation pathways inside the regional domain.
"$CODE_DIR/region_cut_bifway"
# Derive regional river sequence, basin, and upstream/network lookup maps.
"$CODE_DIR/region_set_map"

# Crop each listed high-resolution source dataset when location.txt exists.
HDIRS="1min 30sec 15sec 3sec"
for HIRES in $HDIRS
do
  if [ -f "$SOURCE/$HIRES/location.txt" ]; then
    mkdir -p "../$HIRES"
    "$CODE_DIR/region_combine_hires" "$HIRES"
  fi
done

# Build the parameterization input matrix for channel width/depth calculations.
# This uses sample annual-mean runoff in map/data; it is not a simulation runoff
# input matrix. The simulation-specific matrix is prepared later under src_param.
if [ -f ../1min/location.txt ]; then
  "$CODE_DIR/region_generate_inpmat" 1min
elif [ -f ../30sec/location.txt ]; then
  "$CODE_DIR/region_generate_inpmat" 30sec
elif [ -f ../15sec/location.txt ]; then
  "$CODE_DIR/region_generate_inpmat" 15sec
elif [ -f ../3sec/location.txt ]; then
  "$CODE_DIR/region_generate_inpmat" 3sec
fi

# CTL generation is a separate manual step: run s02-write_ctl.sh after
# reviewing the regionalization outputs. It is not invoked automatically.
