#!/bin/sh
set -eu

# Run from the map-local script_region directory. Regionalization programs use
# paths relative to this working directory.

# Regional source map and requested geographic extent.
SOURCE="../../glb_15min/"  # Source map directory relative to the map-local script_region CWD; keep trailing /.
WEST="90.0"                # Western boundary (degrees longitude).
EAST="110.0"               # Eastern boundary (degrees longitude).
SOUTH="5.0"                # Southern boundary (degrees latitude).
NORTH="35.0"               # Northern boundary (degrees latitude).

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

# ${0##*/} removes the directory portion of the script path for readable messages.
SCRIPT_NAME=${0##*/}
STEP="Prepare regional map workflow"
# The exit trap reports failures and preserves status; signal traps use standard nonzero statuses.
trap '
    EXIT_STATUS=$?
    trap - 0
    if [ "$EXIT_STATUS" -ne 0 ]; then
        printf "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
        printf "!!! ERROR: %s FAILED !!!\n" "$SCRIPT_NAME" >&2
        printf "!!! Step: %s\n" "$STEP" >&2
        printf "!!! Exit status: %s\n" "$EXIT_STATUS" >&2
        printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" >&2
    fi
    exit "$EXIT_STATUS"
' 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

# ----------------------------------------------------------------------
# End of error handling.
# ----------------------------------------------------------------------

# Shared executables must be built beforehand with map/tools/code/Makefile.
CODE_DIR="../../tools/code"
EXEC_CUT_DOMAIN="$CODE_DIR/region_cut_domain"
EXEC_CUT_BIFWAY="$CODE_DIR/region_cut_bifway"
EXEC_SET_MAP="$CODE_DIR/region_set_map"
EXEC_COMBINE_HIRES="$CODE_DIR/region_combine_hires"
EXEC_DEFAULT_INPMAT="$CODE_DIR/region_default_inpmat"

# Save the source map and requested bounds for the regionalization programs.
# Output: region_info.txt (source path, west, east, south, north).
STEP="Save source map and regional bounds"
printf '\n==================================================\n[STEP 1/6] Save source map and regional bounds\n==================================================\n\n'
printf '%s\n' "$SOURCE" "$WEST" "$EAST" "$SOUTH" "$NORTH" > region_info.txt

# Crop the source map and write regional map files and dimension metadata.
STEP="Crop the source map to the requested region"
printf '\n==================================================\n[STEP 2/6] Crop the source map\n==================================================\n\n'
"$EXEC_CUT_DOMAIN"

# Keep only bifurcation pathways inside the regional domain.
STEP="Keep bifurcation pathways inside the regional domain"
printf '\n==================================================\n[STEP 3/6] Crop bifurcation pathways\n==================================================\n\n'
"$EXEC_CUT_BIFWAY"

# Derive regional river sequence, basin, and upstream/network lookup maps.
STEP="Derive regional river and basin maps"
printf '\n==================================================\n[STEP 4/6] Derive regional river and basin maps\n==================================================\n\n'
"$EXEC_SET_MAP"

# Crop each listed high-resolution source dataset when location.txt exists.
# Split the supported source tags into one high-resolution directory per loop.
HDIRS="1min 30sec 15sec 3sec"
for HIRES in $HDIRS
do
  if [ -f "$SOURCE/$HIRES/location.txt" ]; then
    STEP="Crop high-resolution map data for $HIRES"
    printf '\n==================================================\n[STEP 5/6] Crop high-resolution data: %s\n==================================================\n\n' "$HIRES"
    mkdir -p "../$HIRES"
    "$EXEC_COMBINE_HIRES" "$HIRES"
  fi
done

# Build the standard runoff-climatology mapping matrix from the first available
# high-resolution map. The simulation-specific matrix is prepared in script_params.
INPMAT_HIRES=""
if [ -f ../1min/location.txt ]; then
  INPMAT_HIRES="1min"
elif [ -f ../30sec/location.txt ]; then
  INPMAT_HIRES="30sec"
elif [ -f ../15sec/location.txt ]; then
  INPMAT_HIRES="15sec"
elif [ -f ../3sec/location.txt ]; then
  INPMAT_HIRES="3sec"
fi

if [ -n "$INPMAT_HIRES" ]; then
  STEP="Build the standard runoff-climatology input matrix from $INPMAT_HIRES map data"
  printf '\n==================================================\n[STEP 6/6] Build standard input matrix from %s data\n==================================================\n\n' "$INPMAT_HIRES"
  "$EXEC_DEFAULT_INPMAT" "$INPMAT_HIRES"
fi

# CTL generation is a separate manual step: run s02-write_ctl.sh after
# reviewing the regionalization outputs. It is not invoked automatically.
printf '\n==================================================\n[SUCCESS] %s\n==================================================\n' "$SCRIPT_NAME"
