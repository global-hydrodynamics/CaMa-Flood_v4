#!/bin/sh
# Extract directed river-node coordinate pairs for a geographic rectangle.
# Run from a prepared map-local script_analysis/; output includes a column header.
# Inputs: params.txt, nextxy.bin, uparea.bin, elevtn.bin, and lonlat.bin.
set -eu

SCRIPT_DIR=$(CDPATH= cd "$(dirname "$0")" && pwd -P)

# User settings: WEST EAST SOUTH NORTH in degrees; this rectangle covers Mekong.
WEST=90
EAST=110
SOUTH=5
NORTH=35
OUTPUT_FILE="$SCRIPT_DIR/../river_node_latlon_${WEST}_${EAST}_${SOUTH}_${NORTH}.txt"

# ----------------------------------------------------------------------
# User settings end here. Program paths and processing steps follow.
# ----------------------------------------------------------------------

EXEC_RIVER_NODE="$SCRIPT_DIR/analysis_river_node_latlon"
cd "$SCRIPT_DIR"
printf '\n==================================================\n'
printf '[STEP 1/1] Extract river-node coordinate pairs in %s %s %s %s\n' \
    "$WEST" "$EAST" "$SOUTH" "$NORTH"
printf '==================================================\n\n'
"$EXEC_RIVER_NODE" "$WEST" "$EAST" "$SOUTH" "$NORTH" > "$OUTPUT_FILE"
printf 'Output includes one header line followed by data rows: %s\n' "$OUTPUT_FILE"
printf '\n==================================================\n'
printf '[SUCCESS] River-node coordinate extraction complete.\n'
printf '==================================================\n'
